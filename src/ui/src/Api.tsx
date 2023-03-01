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

export type Field = {
  name: string;
  fieldType: Measurement;
}

export type Cube = {
  cQuery: QueryHash;
  cTitle: string;
  cFields: Field[];
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
  visFieldNames: string[];
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

const vis: (args: {q: QueryHash, selections: Selections<Field>}) => Observable<Visualisation[]> = ({q, selections}) =>
  fromFetch(new Request("/api/vis/"+q, { method: "POST", body: JSON.stringify(selections), headers: { "content-type": "application/json" } }))
    .pipe(
      mergeMap(val => val.json().then(vl => vl as Visualisation[])),
      shareReplay(1)
    )

const evl: (arg: {q: QueryHash, fields: string[] }) => Observable<any[]> = ({q, fields}) =>
  fromFetch(new Request("/api/eval/"+q, { method: "POST", body: JSON.stringify(fields), headers: { "content-type": "application/json" } }))
    .pipe(
      mergeMap(val => val.json().then(vl => vl as any[])),
      shareReplay(1)
    )

export default { cubes, cube, vis, evl }