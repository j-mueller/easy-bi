import React, { ReactNode, useEffect, useState } from "react";
import Api, { Hashed, View } from "../Api";
import { Link, Outlet, Route, Routes } from "react-router-dom";
import Page from "./Page";

const ViewList: React.FC = () => {
    const [availableViews, setAvailableViews] = useState<Hashed<View>[]>([]);

    useEffect(() => {
        const sub = Api.views.subscribe(x => setAvailableViews(x));
        return () => sub.unsubscribe();
    })

    function mkViewRow(view: Hashed<View>): ReactNode {
        return <li className="hover:bg-gray-200" key={view[0]}>
                <Link to={"/views/" + view[0]} className="block">
                    {view[1].vTitle}
                </Link>
            </li>;
    }
    return <Page navs={[]} title={<span>Views</span>}>
        <ul className="divide-y divide-gray-200 h-48">
            {availableViews.map(mkViewRow)}
        </ul>
        </Page>
}
const Views: React.FC<{}> = () => {
    return <Routes>
        <Route path="/" element={<Outlet/>}>
            <Route path=":viewid/*" element={<div>view</div>}/>
            <Route index element={<ViewList/>} />
        </Route>
    </Routes>
}

export default Views;