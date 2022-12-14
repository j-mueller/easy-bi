CREATE TABLE sales(
  "ORDERNUMBER" INTEGER,
  "QUANTITYORDERED" NUMERIC,
  "PRICEEACH" NUMERIC,
  "ORDERLINENUMBER" INTEGER,
  "SALES" TEXT,
  "ORDERDATE" TEXT,
  "STATUS" TEXT,
  "QTR_ID" TEXT,
  "MONTH_ID" TEXT,
  "YEAR_ID" TEXT,
  "PRODUCTLINE" TEXT,
  "MSRP" TEXT,
  "PRODUCTCODE" TEXT,
  "CUSTOMERNAME" TEXT,
  "PHONE" TEXT,
  "ADDRESSLINE1" TEXT,
  "ADDRESSLINE2" TEXT,
  "CITY" TEXT,
  "STATE" TEXT,
  "POSTALCODE" TEXT,
  "COUNTRY" TEXT,
  "TERRITORY" TEXT,
  "CONTACTLASTNAME" TEXT,
  "CONTACTFIRSTNAME" TEXT,
  "DEALSIZE" TEXT
);

CREATE VIEW total_sales AS
  SELECT SUM(sales.QUANTITYORDERED * sales.PRICEEACH) as totalSales
  FROM sales

CREATE VIEW total_sales_by_country AS
  SELECT
    SUM(sales."QUANTITYORDERED" * sales."PRICEEACH") as totalSales,
    sales."COUNTRY" as country
  FROM sales
  GROUP BY country

CREATE VIEW sales_fail AS
  SELECT
    SUM(sales."QUANTITYORDERED" * sales."ADDRESSLINE2") as totalSales
  FROM sales
