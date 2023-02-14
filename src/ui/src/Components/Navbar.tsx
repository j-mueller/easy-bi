import React, { useState, useEffect } from "react";
import { HiChevronDoubleLeft, HiChevronDoubleRight, HiOutlineDatabase, HiOutlineHome } from "react-icons/hi";
import { Link, useMatch, useResolvedPath } from "react-router-dom";
import { map, Observable } from "rxjs";
import StatusBar from "./StatusBar";
import Api from "../Api";

type SubItem = { name: string, linkTo: string }

type NavItem = { name: string, icon: React.FC<{ className: string }>, linkTo: string, subItems?: Observable<SubItem[]> }

function biViewLink(viewId: string): string { return `/views/${viewId}` }

const allItems: NavItem[] =
    [{ name: "Home", icon: HiOutlineHome, linkTo: "/" }
        , { name: "Views", icon: HiOutlineDatabase, linkTo: "/views", subItems: Api.views.pipe(map(views => views.map(view => ({ name: view[1].vTitle, linkTo: biViewLink(view[0]) })))) }
    ]

const SubLink: React.FC<{ item: SubItem }> = ({ item }) => {
    const resolved = useResolvedPath(item.linkTo);
    const match = useMatch({ path: resolved.pathname, end: false });
    const prefix = match ? "bg-white text-gray-600 hover:bg-gray-50 hover:text-gray-900" : "bg-gray-100 text-gray-900"
    const className = prefix + " group w-full flex items-center pl-10 pr-2 py-2 text-sm font-medium text-gray-600 rounded-md hover:text-gray-900 hover:bg-gray-50"
    return <Link className={className} to={item.linkTo}>{item.name}</Link>
}

const NavLink: React.FC<{ item: NavItem }> = ({ item }) => {
    const [subItems, setSubItems] = useState<SubItem[]>([]);
    const resolved = useResolvedPath(item.linkTo);
    const match = useMatch({ path: resolved.pathname, end: (item.linkTo === "/") });
    useEffect(() => {
        if (item.subItems) {
            const sub = item.subItems.subscribe(setSubItems);
            return () => sub.unsubscribe();
        } else {
            return () => { };
        }
    }, [])

    function itemStyle(): string {
        const pre = (match) ? "bg-eucalyptus-600 text-white" : "text-gray-600 hover:bg-gray-50 hover:text-gray-900";
        return pre + " group flex items-center px-2 py-2 text-base font-medium rounded-md"
    }
    function iconClass(): string {
        const pre = (match) ? "text-gray-100" : "text-gray-400 group-hover:text-gray-500"
        return pre + " mr-3 h-6 w-6"
    }
    return <div className="space-y-1">
        <Link to={item.linkTo} className={itemStyle()}>
            <item.icon className={iconClass()} />
            {item.name}
        </Link>
        {(match) ? <ol>{subItems.map(su => <li key={su.linkTo}><SubLink item={su} /></li>)}</ol> : <ol />}
    </div>
}

const Navbar: React.FC = () => {
    const [isExpanded, setIsExpanded] = useState<boolean>(true);
    return <div className="flex flex-shrink-0 select-none">
        {(isExpanded) ?
            (<div className="flex flex-col w-64">
                <div className="flex-1 flex flex-col min-h-0 border-r border-gray-200 bg-gray-100">
                    <div className="flex-1 flex flex-col pb-4 overflow-y-auto">
                        <div className="flex items-center flex-shrink-0 px-4 bg-eucalyptus-600 h-16 text-white text-xl" onClick={() => setIsExpanded(false)}>
                            <div className="flex-grow">Easy-BI</div>
                            <button><HiChevronDoubleLeft className="text-gray-200 hover:text-gray-100 h-6 w-6" /></button>
                        </div>
                        <nav className="mt-5 flex-1" aria-label="sidebar">
                            <div className="px-2 space-y-1">
                                {allItems.map(item => { return <NavLink item={item} /> })}
                            </div>
                        </nav>
                    </div>
                    <div className="flex-shrink-0 flex border-t border-dark-blue p-4">
                        <StatusBar status="green" />
                    </div>
                </div>
            </div>) : (<div className="flex flex-col w-8 bg-eucalyptus-600">
                <div className="flex-1 flex flex-col min-h-0 border-r border-gray-200">
                    <div className="mx-1 mt-5 flex-1">
                        <button onClick={() => setIsExpanded(true)}>
                            <HiChevronDoubleRight className="text-gray-200 hover:text-eucalyptus-100 h-6 w-6" />
                        </button>
                    </div>
                </div>
            </div>)
        }
    </div>

}

export default Navbar;