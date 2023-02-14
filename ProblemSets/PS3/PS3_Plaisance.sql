CREATE TABLE florida(
policyID INTEGER,
statecode CHAR,
county CHAR,
eq_site_limit INTEGER,
hu_site_limit INTEGER,
fl_site_limit INTEGER,
fr_site_limit INTEGER,
tiv_2011 INTEGER,
tiv_2012 INTEGER,
eq_site_deductible INTEGER,
hu_site_deductible INTEGER,
fl_site_dedictible INTEGER,
fr_site_deductible INTEGER,
point_latitude INTEGER,
point_longitude INTEGER,
line CHAR,
construction CHAR,
point_granularity INTEGER
);
.mode csv
.import FL_insurance_sample.csv florida
SELECT * FROM florida LIMIT 10;
SELECT DISTINCT county FROM florida;
SELECT AVG(tiv_2012-tiv_2011) FROM florida;
SELECT construction, COUNT(*) FROM florida GROUP BY construction;
