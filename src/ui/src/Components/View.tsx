import React, { ReactNode, useEffect, useState } from "react";
import Api, { Archetype, Visualisation } from "../Api";
import { useParams } from "react-router-dom";
import Page from "./Page";
import { combineLatestWith, mergeMap, Subject, tap } from "rxjs";
import { VegaLite } from "react-vega";
import { HiViewGrid } from "react-icons/hi";
import { AiOutlineDotChart, AiOutlineHeatMap, AiOutlineLineChart } from "react-icons/ai";
import { TfiBarChart } from "react-icons/tfi";

const ViewTitle: React.FC<{ viewId: string }> = ({ viewId }) => {
  const [viewTitle, setViewTitle] = useState<string>(viewId);
  useEffect(() => {
    const sub = Api.view(viewId).subscribe(view => setViewTitle(view.vTitle));
    return () => sub.unsubscribe();
  }, [viewId])

  return <span>{viewTitle}</span>

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

const ViewPage: React.FC<{ viewId: string }> = ({ viewId }) => {
  const [vis, setVis] = useState<Visualisation[]>([]);
  const [visSubj] = useState<Subject<Visualisation>>(new Subject<Visualisation>());
  const [visComp, setVisComp] = useState<JSX.Element>(<div className="flex flex-grow">No selection</div>);

  useEffect(() => {
    const viewDetails = Api.view(viewId);
    const sub = viewDetails.pipe(mergeMap(view => Api.vis(view.vQuery)), tap(x => { ((vis.length == 0) && (x.length > 0)) ? visSubj.next(x[0]) : {} })).subscribe(setVis);
    const queryResult = viewDetails.pipe(mergeMap(v => Api.evl(v.vQuery)));

    const sub2 =
      visSubj
        .pipe(combineLatestWith(queryResult))
        .subscribe(([view, dt]) => setVisComp(<VegaLite className="flex-grow flex border border-gray-200" spec={view.visDefinition} actions={false} data={{ table: dt }} />));

    const sub3 = visSubj.subscribe(console.log);

    return () => { sub.unsubscribe(); sub2.unsubscribe(); sub3.unsubscribe(); }
  }, [viewId]);

  function mkVisRow(vis: Visualisation): ReactNode {
    return <li
      className="cursor-pointer w-32 m-2 bg-slate-300 hover:bg-eucalyptus-300 hover:border-eucalyptus-700 border-slate-700 border-2 h-32 flex place-items-center"
      onClick={() => visSubj.next(vis)}
    >
      <ArchetypeC archetype={vis.visArchetype}/>
    </li>;
  }

  return <Page navs={[]} title={<ViewTitle viewId={viewId} />}>
    <div className="flex flex-col flex-1">
      <ul className="flex flex-row h-32">{vis.map(mkVisRow)}</ul>
      <div className="flex-grow flex m-12">
        {visComp}
      </div>
    </div>
  </Page>
}

const ViewComponent: React.FC = () => {
  const params = useParams();
  const viewId = params["viewid"] || "";
  if (viewId === "") { console.warn("ViewPage: View ID not found in URL params") };
  return <ViewPage viewId={viewId} />
}

export default ViewComponent;