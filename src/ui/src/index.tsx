import React from "react";
import ReactDOM from "react-dom";
import { createRoot } from 'react-dom/client';
import App from "./App";
import Home from "./Components/Home";
import Cubes from "./Components/Cubes";
import { createBrowserRouter, createRoutesFromElements, Link, Route, RouterProvider, Routes } from "react-router-dom";

import './index.css';

const rootElem = document.getElementById("root");
const actual: HTMLElement = rootElem ? rootElem : document.createElement("div");
if (!rootElem) { console.warn("Could not find root element") };
const root = createRoot(actual);

const router = createBrowserRouter(
  createRoutesFromElements(
    <Route path="/" element={<App />}>
      <Route index element={<Home />} />
      <Route
        path="cubes/*"
        element={<Cubes />}
        handle={{
          crumb: () => <Link to="/cubes">Cubes</Link>
        }}
      />
    </Route>
  )
)

root.render(
  <React.StrictMode>
    <RouterProvider router={router} />
  </React.StrictMode>);