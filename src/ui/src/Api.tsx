import { merge, shareReplay, Subject } from "rxjs";
import { fromFetch } from "rxjs/fetch"
import { Observable } from "rxjs/internal/Observable";
import { map, mergeMap, scan } from "rxjs/operators";

export type Hash = string;

export type Hashed<T> = [Hash, T]

export type View = {
    vQuery: Hash;
    vTitle: string;
}

const views: Observable<Hashed<View>[]> =
    fromFetch("/api/views")
    .pipe(
        mergeMap(val => val.json().then(vl => vl as Hashed<View>[])),
        shareReplay(1)
    )

export default { views }