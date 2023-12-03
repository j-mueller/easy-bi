import React, { useEffect, useState } from "react";
import Page from "./Page";
import Api, { WithHash, Cube } from "../Api";
import { Link } from "react-router-dom";

const CubeList: React.FC<{}> = () => {
  const [cubes, setCubes] = useState<WithHash<Cube>[]>([]);

  useEffect(() => {
    const sub = Api.cubes.subscribe(setCubes);
    return () => sub.unsubscribe();
  }, [])

  function mkCubeRow(c: WithHash<Cube>) {
    const cls = "relative flex items-center space-x-3 rounded-lg border border-gray-300 bg-white px-6 py-5 shadow-sm focus-within:ring-2 focus-within:ring-indigo-500 focus-within:ring-offset-2 hover:border-eucalyptus-400 cursor-pointer"
    return <div key={c[0]} className={cls}>
      <div className="flex-shrink-0">
        <img></img>
      </div>
      <div className="min-w-0 flex-1">
        <Link to={"/cubes/" + c[0]}>
          <p className="text-sm font-medium text-gray-900">{c[1].name}</p>
          <p className="truncate text-sm text-gray-500">{c[1].dimensions.map(f => f[1].name).join(", ")}</p>
        </Link>

      </div>
      </div>

  }

  return <Page title={<h1>Datasets</h1>} navs={[]}>
    <div>
      <div className="grid grid-cols-1 gap-4 sm:grid-cols-2 m-4">
      {cubes.map(mkCubeRow)}
      </div>
    </div>
  </Page>
}

const Home: React.FC<{}> = () => {
  return <CubeList />
}

export default Home;