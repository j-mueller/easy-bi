import React from "react";
import ReactDOM from "react-dom";
import App from "./App";
import Home from "./Components/Home";
import Cubes from "./Components/Cubes";
import { BrowserRouter, Route, Routes } from "react-router-dom";

import './index.css';

ReactDOM.render(
    <BrowserRouter>
        <Routes>
            <Route path="/" element={<App />}>
                <Route index element={<Home/>}/>
                <Route path="cubes/*" element={<Cubes/>}/>
            </Route>
        </Routes>
    </BrowserRouter>, document.getElementById("root"));