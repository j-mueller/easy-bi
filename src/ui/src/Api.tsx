import { merge, shareReplay, Subject } from "rxjs";
import { fromFetch } from "rxjs/fetch"
import { Observable } from "rxjs/internal/Observable";
import { map, mergeMap, scan } from "rxjs/operators";
import { VisualizationSpec } from "react-vega";

export type QueryHash = string;

export type ViewHash = string;

export type Hashed<T> = [ViewHash, T]

export type View = {
  vQuery: QueryHash;
  vTitle: string;
}

export type Visualisation = {
  visDefinition: VisualizationSpec;
  visDescription: string;
  visScore: number;
}

const views: Observable<Hashed<View>[]> =
  fromFetch("/api/views")
    .pipe(
      mergeMap(val => val.json().then(vl => vl as Hashed<View>[])),
      shareReplay(1)
    )

const view: (v: Hash) => Observable<View> = (v: string) =>
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

export default { views, view, vis }