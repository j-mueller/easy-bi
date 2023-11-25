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

-- Sales by country and product line over time
CREATE VIEW "Sales" AS
  SELECT
    SUM(sales."QUANTITYORDERED" * sales."PRICEEACH") as totalSales,
    SUM(sales."PRICEEACH") as totalPriceEach,
    COUNT(*) as count,
    sales."COUNTRY" as country,
    sales."STATUS" as status,
    sales."PRODUCTLINE" as productline,
    sales."TERRITORY" as territory,
    sales."PRODUCTCODE" as productcode,
    sales."CUSTOMERNAME" as customername,
    sales."DEALSIZE" as dealsize
  FROM sales