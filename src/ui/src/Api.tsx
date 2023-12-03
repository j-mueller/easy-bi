import { shareReplay } from "rxjs";
import { fromFetch } from "rxjs/fetch"
import { Observable } from "rxjs/internal/Observable";
import { mergeMap } from "rxjs/operators";
import { VisualizationSpec } from "react-vega";

export type QueryHash = string;

export type CubeHash = string;

export type WithHash<T> = [CubeHash, T]

export type Measurement =
  "Nominal"
  | "Ordinal"
  | "Quantitative"
  | "TemporalAbs"
  | "TemporalRel"

function isQuantitative(measurement: Measurement): boolean {
  return (measurement === "Quantitative");
}

export type SortOrder =
  "Ascending"
  | "Descending"
  | "None"

export type Filter =
  { tag: "TopN", n: number}
  | { tag: "IncludeList", include: string[]}
  | {tag: "NoFilter"}

export type SqlFieldName = string;

export type Field = {
  sqlFieldName: SqlFieldName;
  displayName?: string;
  fieldType: Measurement;
};

/**
 * 
 * @param fieldGroup The field group
 * @returns All `OutField` values of the field group
 */
function allOutFields(fieldGroup: DimensionGroup): Dimension[] {
  return ([fieldGroup.primary_dimension]).concat(fieldGroup.other_dimensions);
}

export type Dimension = {
  sql_field_name: SqlFieldName;
  display_name?: string;
  type: Measurement;
  default_sort_order: SortOrder;
}

export type Measure = {
  sql_field_name: SqlFieldName;
  display_name?: string;
}

export type DimensionGroup = {
  name: string;
  description?: string;
  primary_dimension: Dimension;
  other_dimensions: Dimension[];
}

export type Cube = {
  query: QueryHash;
  name: string;
  display_name: string;
  dimensions: WithHash<DimensionGroup>[];
  measures: WithHash<Measure>[];
}

export type Archetype =
  "HorizontalBarChart"
  | "VerticalBarChart"
  | "Linechart"
  | "Scatterplot"
  | "Heatmap"
  | "Misc";

export type Visualisation = {
  visDefinition: VisualizationSpec;
  visDescription: string;
  visScore: number;
  visArchetype: Archetype;
  visFields: Field[];
  visApiQuery: APIQuery;
  visQuery: CubeHash;
}

export type Mark = "Bar" | "Point" | "Line" | "Rect"

export type Selections<T> = {
  _WildCards: T[];
  _XAxis: T[];
  _YAxis: T[];
  _YAxis2: T[];
  _Color: T[];
  _selectedMark: Mark[];
  _selectedArchetype: Archetype[];
}

function selections(wildCards: SqlFieldName[]): Selections<SqlFieldName> {
  return {
    _WildCards: wildCards,
    _Color: [],
    _XAxis: [],
    _YAxis: [],
    _YAxis2: [],
    _selectedArchetype: [],
    _selectedMark: []
  }
}

const cubes: Observable<WithHash<Cube>[]> =
  fromFetch("/api/cubes")
    .pipe(
      mergeMap(val => val.json().then(vl => vl as WithHash<Cube>[])),
      shareReplay(1)
    )

const cube: (v: CubeHash) => Observable<Cube> = (v: string) =>
  fromFetch("/api/cubes/" + v)
    .pipe(
      mergeMap(val => val.json().then(vl => vl as Cube)),
      shareReplay(1)
    )

const vis: (args: {q: CubeHash, selections: Selections<SqlFieldName>}) => Observable<WithHash<Visualisation>[]> = ({q, selections}) =>
  fromFetch(new Request("/api/vis/"+q, { method: "POST", body: JSON.stringify(selections), headers: { "content-type": "application/json" } }))
    .pipe(
      mergeMap(val => val.json().then(vl => vl as WithHash<Visualisation>[])),
      shareReplay(1)
    )

export type APIQuery = {
  splits: SqlFieldName[];
  measures: SqlFieldName[];
  filters: [SqlFieldName, Filter][];
  sort_order: [SqlFieldName, SortOrder][];
  }

const evl: (arg: {q: CubeHash, query: APIQuery }) => Observable<any[]> = ({q, query}) =>
  fromFetch(new Request("/api/eval/"+q, { method: "POST", body: JSON.stringify(query), headers: { "content-type": "application/json" } }))
    .pipe(
      mergeMap(val => val.json().then(vl => vl as any[])),
      shareReplay(1)
    )

export default { cubes, cube, vis, evl, allOutFields, isQuantitative, selections}