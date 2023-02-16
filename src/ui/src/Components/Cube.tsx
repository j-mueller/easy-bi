import React, { ReactNode, useEffect, useState } from "react";
import Api, { Archetype, Visualisation } from "../Api";
import { useParams } from "react-router-dom";
import Page from "./Page";
import { combineLatestWith, mergeMap, Subject, tap } from "rxjs";
import { VegaLite } from "react-vega";
import { HiViewGrid } from "react-icons/hi";
import { AiOutlineDotChart, AiOutlineHeatMap, AiOutlineLineChart } from "react-icons/ai";
import { TfiBarChart } from "react-icons/tfi";

const CubeTitle: React.FC<{ cubeId: string }> = ({ cubeId }) => {
  const [cubeTitle, setCubeTitle] = useState<string>(cubeId);
  useEffect(() => {
    const sub = Api.cube(cubeId).subscribe(cube => setCubeTitle(cube.cTitle));
    return () => sub.unsubscribe();
  }, [cubeId])

  return <span>{cubeTitle}</span>

}

const ArchetypeC: React.FC<{archetype: Archetype}> = ({archetype}) => {
  const cls = "flex-grow h-16 w-16"
  switch (archetype) {
    case "HorizontalBarChart":
      return <TfiBarChart className={cls + " rotate-90"}/>
    case "VerticalBarChart":
      return <TfiBarChart className={cls  }/>
    case "Heatmap":
      return <AiOutlineHeatMap className={cls}/>;
    case "Linechart":
      return <AiOutlineLineChart className={cls}/>
    case "Scatterplot":
      return <AiOutlineDotChart className={cls}/>
    default:
      return <HiViewGrid className={cls}/>
  }
}

const CubePage: React.FC<{ cubeId: string }> = ({ cubeId }) => {
  const [vis, setVis] = useState<Visualisation[]>([]);
  const [visSubj] = useState<Subject<Visualisation>>(new Subject<Visualisation>());
  const [visComp, setVisComp] = useState<JSX.Element>(<div className="flex flex-grow">No selection</div>);

  useEffect(() => {
    const viewDetails = Api.cube(cubeId);
    const sub = viewDetails.pipe(mergeMap(cube => Api.vis(cube.cQuery)), tap(x => { ((vis.length == 0) && (x.length > 0)) ? visSubj.next(x[0]) : {} })).subscribe(setVis);
    const queryResult = viewDetails.pipe(mergeMap(v => Api.evl(v.cQuery)));

    const sub2 =
      visSubj
        .pipe(combineLatestWith(queryResult))
        .subscribe(([view, dt]) => setVisComp(<VegaLite className="flex-grow flex border border-gray-200" spec={view.visDefinition} actions={false} data={{ table: dt }} />));

    const sub3 = visSubj.subscribe(console.log);

    return () => { sub.unsubscribe(); sub2.unsubscribe(); sub3.unsubscribe(); }
  }, [cubeId]);

  function mkVisRow(vis: Visualisation): ReactNode {
    return <li
      className="cursor-pointer w-32 m-2 bg-slate-300 hover:bg-eucalyptus-300 hover:border-eucalyptus-700 border-slate-700 border-2 h-32 flex place-items-center"
      onClick={() => visSubj.next(vis)}
    >
      <ArchetypeC archetype={vis.visArchetype}/>
    </li>;
  }

  return <Page navs={[]} title={<CubeTitle cubeId={cubeId} />}>
    <div className="flex flex-col flex-1">
      <ul className="flex flex-row h-32">{vis.map(mkVisRow)}</ul>
      <div className="flex-grow flex m-12">
        {visComp}
      </div>
    </div>
  </Page>
}

const CubeComponent: React.FC = () => {
  const params = useParams();
  const cubeId = params["cubeid"] || "";
  if (cubeId === "") { console.warn("CubePage: Cube ID not found in URL params") };
  return <CubePage cubeId={cubeId} />
}

export default CubeComponent;