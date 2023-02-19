import React, { ReactNode, useEffect, useState } from "react";
import Api, { Hashed, Cube } from "../Api";
import { Link, Outlet, Route, Routes } from "react-router-dom";
import Page from "./Page";
import CubeComponent from "./Cube";

const CubeList: React.FC = () => {
    const [availableCubes, setAvailableCubes] = useState<Hashed<Cube>[]>([]);

    useEffect(() => {
        const sub = Api.cubes.subscribe(x => setAvailableCubes(x));
        return () => sub.unsubscribe();
    })

    function mkCubeRow(cube: Hashed<Cube>): ReactNode {
        return <li className="hover:bg-gray-200" key={cube[0]}>
                <Link to={"/views/" + cube[0]} className="block">
                    {cube[1].cTitle}
                </Link>
            </li>;
    }
    return <Page navs={[]} title={<span>Cubes</span>}>
        <ul className="divide-y divide-gray-200 h-48">
            {availableCubes.map(mkCubeRow)}
        </ul>
        </Page>
}
const Cubes: React.FC<{}> = () => {
    return <Routes>
        <Route path="/" element={<Outlet/>}>
            <Route path=":cubeid/*" element={<CubeComponent/>}/>
            <Route index element={<CubeList/>} />
        </Route>
    </Routes>
}

export default Cubes;