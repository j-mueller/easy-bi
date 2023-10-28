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

export type SortOrder =
  "Ascending"
  | "Descending"
  | "None"

  export type NominalInFilter
  = { tag: "list"; values: string[] }
  | { tag: "top-n"; n: number }
  | { tag: "NoFilter" }

export type InFieldOptions
  = { "tag": "nominal"; filter: NominalInFilter }
  | { "tag": "ordinal"; filter: GenericNoFilter }
  | { "tag": "quantitative"; filter: GenericNoFilter }
  | { "tag": "temporal-abs"; filter: GenericNoFilter }
  | { "tag": "temporal-rel"; filter: GenericNoFilter }

export type InField = {
  sql_field_name: string;
  display_name: string;
  field_options: InFieldOptions;
  sort_order: SortOrder;
}

export type NominalOutFilter
  = { tag: "list"; values: string[] }
  | { tag: "top-n"; n: number }
  | { tag: "NoFilter" }

export type GenericNoFilter =
  { tag: "NoFilter" }

export type OutFieldOptions
  = { tag: "nominal"; filter: NominalOutFilter }
  | { tag: "ordinal"; filter: GenericNoFilter }
  | { tag: "quantitative"; filter: GenericNoFilter }
  | { tag: "temporal-abs"; filter: GenericNoFilter }
  | { tag: "temporal-rel"; filter: GenericNoFilter }

export type OutField = {
  sql_field_name: string;
  display_name?: string;
  field_options: OutFieldOptions;
  sort_order: SortOrder;
};

function mkInField(outField: OutField): InField {
  const tg = outField.field_options.tag;
  const options: InFieldOptions = tg == "nominal" ? { tag: "nominal", filter: { tag: "top-n", n: 10}} : { tag: tg, filter: {tag: "NoFilter"}};
  return { sql_field_name: outField.sql_field_name
         , display_name: outField.display_name ? outField.display_name : outField.sql_field_name
         , field_options: options
         , sort_order: outField.sort_order
        }
}

export type FieldGroup = {
  name: string;
  description?: string;
  primary_field: OutField;
  other_fields: OutField[];
}

export type Cube = {
  query: QueryHash;
  name: string;
  display_name: string;
  fields: FieldGroup[];
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
  visFields: InField[];
  visQuery: QueryHash;
}

export type Mark = "Bar" | "Point" | "Line" | "Rect"

export type Selections<T> = {
  _WildCards: T[];
  _XAxis: T[];
  _YAxis: T[];
  _Color: T[];
  _selectedMark: Mark[];
  _selectedArchetype: Archetype[];
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

const vis: (args: {q: QueryHash, selections: Selections<InField>}) => Observable<WithHash<Visualisation>[]> = ({q, selections}) =>
  fromFetch(new Request("/api/vis/"+q, { method: "POST", body: JSON.stringify(selections), headers: { "content-type": "application/json" } }))
    .pipe(
      mergeMap(val => val.json().then(vl => vl as WithHash<Visualisation>[])),
      shareReplay(1)
    )

const evl: (arg: {q: QueryHash, fields: InField[] }) => Observable<any[]> = ({q, fields}) =>
  fromFetch(new Request("/api/eval/"+q, { method: "POST", body: JSON.stringify(fields), headers: { "content-type": "application/json" } }))
    .pipe(
      mergeMap(val => val.json().then(vl => vl as any[])),
      shareReplay(1)
    )

export default { cubes, cube, vis, evl, mkInField }