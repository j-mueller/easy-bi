import { shareReplay } from "rxjs";
import { fromFetch } from "rxjs/fetch"
import { Observable } from "rxjs/internal/Observable";
import { mergeMap } from "rxjs/operators";
import { VisualizationSpec } from "react-vega";

export type QueryHash = string;

export type ViewHash = string;

export type Hashed<T> = [ViewHash, T]

export type View = {
  vQuery: QueryHash;
  vTitle: string;
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
}

const views: Observable<Hashed<View>[]> =
  fromFetch("/api/views")
    .pipe(
      mergeMap(val => val.json().then(vl => vl as Hashed<View>[])),
      shareReplay(1)
    )

const view: (v: ViewHash) => Observable<View> = (v: string) =>
  fromFetch("/api/views/" + v)
    .pipe(
      mergeMap(val => val.json().then(vl => vl as View)),
      shareReplay(1)
    )

const vis: (q: QueryHash) => Observable<Visualisation[]> = (q: QueryHash) =>
  fromFetch("/api/vis/" + q)
    .pipe(
      mergeMap(val => val.json().then(vl => vl as Visualisation[])),
      shareReplay(1)
    )

const evl: (q: QueryHash) => Observable<any[]> = (q: QueryHash) =>
  fromFetch("/api/eval/" + q)
    .pipe(
      mergeMap(val => val.json().then(vl => vl as any[])),
      shareReplay(1)
    )

export default { views, view, vis, evl }