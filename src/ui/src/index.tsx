import React from "react";
import ReactDOM from "react-dom";
import App from "./App";
import Home from "./Components/Home";
import Cubes from "./Components/Cubes";
import { createBrowserRouter, createRoutesFromElements, Link, Route, RouterProvider, Routes } from "react-router-dom";

import './index.css';

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

ReactDOM.render(
  <React.StrictMode>
    <RouterProvider router={router} />
  </React.StrictMode>
  , document.getElementById("root"));