USE funkysdb;

CREATE TABLE rough_customers (
    customer_id bigint primary key,
    first_name varchar(100),
    last_name varchar(100),
    email varchar(200),
    email_opt_in varchar(5),
    company_address varchar(250),
    address1 varchar(200),
    address2 varchar(200),
    city varchar(200),
    province varchar(100),
    country varchar(100),
    postal varchar(100),
    home_phone varchar(100),
    cell_phone varchar(100),
    sms_opt_in varchar(5),
    total_spent decimal(10,2),
    total_orders int,
    note varchar(500),
);

-- * REMEMBER TO DO: SET GLOBAL local_infile = 1; *
LOAD DATA LOCAL INFILE 'filepath.csv'
INTO TABLE rough_customers
FIELDS 
	TERMINATED BY ','
	OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\n'
IGNORE 1 ROWS; 


-- Customer data of interest
CREATE TABLE customers AS
SELECT 
	customer_id,
    first_name,
    last_name,
    email_opt_in,
    city,
    province,
    country,
    cell_phone,
    sms_opt_in,
    total_spent,
    total_orders,
FROM rough_customers;



-- Total customers that are subscribed to email or sms marketing
CREATE TABLE subscribers AS
SELECT * FROM customers
WHERE sms_opt_in = "yes" OR email_opt_in = "yes";
-- Number and percentage of subscribed customers (email/sms marketing)
SELECT 
(SELECT COUNT(*) FROM subscribers) AS total_subs,
CONCAT(FORMAT(((SELECT COUNT(*) FROM subscribers) / (SELECT COUNT(*) FROM customers) * 100), 2), '%')
AS subscriber_pct;



-- Customers that made no purchases
CREATE TABLE nopurchase AS
SELECT * FROM customers 
WHERE total_orders = 0;
-- Total number and percentage of customers who have yet to make a purchase
SELECT
COUNT(*) AS total_nopurchase,
(SELECT COUNT(*) FROM nopurchase) / (SELECT COUNT(*) FROM customers) AS nopurchase_ratio
FROM nopurchase;
-- Total number and percentage of no purchase customers who are subscribers
SELECT 
COUNT(DISTINCT np.customer_id) AS total_nopurchase,
COUNT(DISTINCT sub.customer_id) AS nopurchase_subs,
CONCAT(FORMAT(COUNT(DISTINCT sub.customer_id)/COUNT(DISTINCT np.customer_id)*100, 2), '%') AS pct_nopurchase_subs
FROM nopurchase np
LEFT JOIN subscribers sub ON np.customer_id = sub.customer_id;


-- List of customers in descending order from top spender to lowest 
-- *DID NOT INCLUDE CUSTOMERS WITH NO PURCHASE OR CUSTOMERS WHO HAD ORDERS BUT LESS THAN $0.40 SPENT*
CREATE TABLE spenders AS
SELECT 
customer_id,
first_name,
last_name,
email_opt_in,
city,
province,
country,
cell_phone,
sms_opt_in,
total_orders,
total_spent,
total_spent/NULLIF(total_orders,0) AS avg_spent
FROM customers
WHERE total_orders >=1 AND total_spent >= 0.40 
ORDER BY total_spent ASC;



-- Total number and percentage of customers who made at least one purchase
SELECT 
COUNT(*) AS total_spenders, 
CONCAT(FORMAT((SELECT COUNT(*) FROM spenders)/(SELECT COUNT(*) from customers)*100, 2), '%') AS spenders_ratio
FROM spenders;



-- Orders and numbers the rows based on spending (highest to lowest spenders)
CREATE TABLE spent_numbered AS (
	SELECT *,
		ROW_NUMBER() OVER (ORDER BY total_spent ASC) AS rn,
        COUNT(*) OVER () AS total_rows
	FROM spenders
);



-- Descriptive statistics for spenders
SELECT
CONCAT('$', FORMAT(SUM(total_spent), 2)) AS total_revenue,
CONCAT('$', FORMAT(AVG(total_spent), 2)) AS avg_spent,
CONCAT('$', FORMAT(STDDEV(total_spent), 2)) AS sd_spent,
CONCAT('$', FORMAT(MIN(total_spent),2)) AS min_spent,
CONCAT('$', FORMAT(MAX(total_spent),2)) AS max_spent,

SUM(total_orders) AS total_s_orders,
AVG(total_orders) AS avg_orders,
FORMAT(STDDEV(total_orders), 2) AS sd_orders,
MIN(total_orders) AS min_order,
MAX(total_orders) AS max_order
FROM spent_numbered;


-- Displaying quartile values for total spent among spenders
SELECT * FROM (
SELECT
customer_id,
first_name,
last_name,
total_orders,
total_spent,
COUNT(*) OVER() AS total_spenders,
ROW_NUMBER() OVER(ORDER BY total_spent ASC) AS rn
FROM spenders
) TEMP
WHERE rn IN (round(0.25*total_spenders), round(0.50*total_spenders), round(0.75*total_spenders));



-- Distribution of orders
SELECT
total_orders,
COUNT(*) AS count
FROM spenders
GROUP BY total_orders
ORDER BY total_orders ASC;



-- Top 20% of spenders (based on total spent)
CREATE TABLE top20 AS (
SELECT
rn,
customer_id,
first_name,
last_name,
email,
email_opt_in,
city,
province,
country,
cell_phone,
sms_opt_in,
total_orders,
tax_exempt,
total_spent,
avg_spent,
COUNT(*) OVER() AS total_rows_t20
FROM spent_numbered WHERE rn >= 0.8 * total_rows
ORDER BY total_spent ASC
);
-- Bottom 20%
CREATE TABLE bottom20 AS (
SELECT 
rn,
customer_id,
first_name,
last_name,
email,
email_opt_in,
city,
province,
country,
cell_phone,
sms_opt_in,
total_orders,
tax_exempt,
total_spent,
avg_spent,
COUNT(*) OVER() AS total_rows_b20
FROM spent_numbered WHERE rn <= 0.20 * total_rows
ORDER BY total_spent ASC
);
-- Middle 60%
CREATE TABLE mid60 AS (
SELECT 
rn,
customer_id,
first_name,
last_name,
email,
email_opt_in,
city,
province,
country,
cell_phone,
sms_opt_in,
total_orders,
tax_exempt,
total_spent,
avg_spent,
COUNT(*) OVER() AS total_rows_m60
FROM spent_numbered WHERE rn BETWEEN 0.20 * total_rows AND 0.80 * total_rows
);


-- Gets specified analytics based on spenders list broken into into segments for top 20%, middle 60%, and bottom 20% of spenders
WITH segmented AS (
    SELECT *,
        CASE
            WHEN rn <= 0.2 * total_rows THEN 'Bottom 20%'
            WHEN rn >= 0.8 * total_rows THEN 'Top 20%'
            ELSE 'Middle 60%'
        END AS segment
    FROM spent_numbered
)
SELECT
    segment, -- Labels each row by segment
    ROUND(SUM(s.total_spent),2) AS total_spent, -- Total amount spent
    ROUND(AVG(s.total_spent),2) AS avg_spent, -- Average amount spent
    MAX(s.total_spent) AS largest, -- Highest value order amount
    MIN(s.total_spent) AS lowest, -- Lowest value order amount
    ROUND(AVG(s.total_orders),0) AS avg_orders, -- Average number of orders
   
	-- Share of total revenue
    ROUND(SUM(s.total_spent)/NULLIF((SELECT SUM(total_spent) FROM spenders),0), 4) AS revenue_share, 
    
    -- Number of customers who are tax-exempt
    COUNT(CASE WHEN s.tax_exempt = 'yes' THEN 1 END) AS tax_exempt_count,
    
    -- Percentage of customers who are tax-exempt
    ROUND(COUNT(CASE WHEN s.tax_exempt = 'yes' THEN 1 END)/COUNT(*), 4) AS tax_exempt_pct,
    
    -- Number of customers
    COUNT(DISTINCT s.customer_id) AS total_spenders,
	
    -- Number of customers who are subscribed to email and/or SMS marketing
    COUNT(DISTINCT sub.customer_id) AS subscribed_spenders,
    
    -- Percentage of customers who are subscribed to email and/or SMS marketing
    ROUND(COUNT(DISTINCT sub.customer_id)/COUNT(DISTINCT s.customer_id), 4) AS pct_subscribed
FROM segmented s
LEFT JOIN subscribers sub ON s.customer_id = sub.customer_id

-- The analytics above are specified and organized by segment
GROUP BY segment

-- Order rows from top spenders to bottom spenders
ORDER BY 
    CASE segment
        WHEN 'Top 20%' THEN 1
        WHEN 'Middle 60%' THEN 2
        WHEN 'Bottom 20%' THEN 3
    END;



-- Same as above but for display purposes
SELECT
    segment, -- Labels each row by segment
    CONCAT('$', FORMAT(SUM(s.total_spent), 2)) AS total_spent, -- Total amount spent
    CONCAT('$', FORMAT(AVG(s.total_spent), 2)) AS avg_spent, -- Average amount spent
    CONCAT('$', FORMAT(MAX(s.total_spent), 2)) AS largest, -- Highest value order amount
    CONCAT('$', FORMAT(MIN(s.total_spent), 2)) AS lowest, -- Lowest value order amount
    AVG(s.total_orders) AS avg_orders, -- Average number of orders
   
	-- Share of total revenue
    CONCAT(FORMAT(ROUND(SUM(s.total_spent)/NULLIF((SELECT SUM(total_spent) FROM spenders),0)*100, 2), 2), '%') AS revenue_share, 
    
    -- Number of customers who are tax-exempt
    COUNT(CASE WHEN s.tax_exempt = 'yes' THEN 1 END) AS tax_exempt_count,
    
    -- Percentage of customers who are tax-exempt
    CONCAT(FORMAT(ROUND(COUNT(CASE WHEN s.tax_exempt = 'yes' THEN 1 END)/COUNT(*)*100, 2), 2), '%') AS tax_exempt_pct,
    
    -- Number of customers
    COUNT(DISTINCT s.customer_id) AS total_spenders,
	
    -- Number of customers who are subscribed to email and/or SMS marketing
    COUNT(DISTINCT sub.customer_id) AS subscribed_spenders,
    
    -- Percentage of customers who are subscribed to email and/or SMS marketing
    CONCAT(FORMAT(ROUND(COUNT(DISTINCT sub.customer_id)/COUNT(DISTINCT s.customer_id)*100, 2),2), '%') AS pct_subscribed
FROM segmented s
LEFT JOIN subscribers sub ON s.customer_id = sub.customer_id

-- The analytics above are specified and organized by segment
GROUP BY segment

-- Order rows from top spenders to bottom spenders
ORDER BY 
    CASE segment
        WHEN 'Top 20%' THEN 1
        WHEN 'Middle 60%' THEN 2
        WHEN 'Bottom 20%' THEN 3
    END;


-- Get average amount spent by and amount of subscribers vs. non-subscribers 
WITH subspenders AS (
	    SELECT *,
        CASE
            WHEN sms_opt_in = "yes" OR email_opt_in = "yes" THEN 'Subscribed'
            ELSE 'Not Subscribed'
        END AS sub
    FROM spenders -- Only includes customers who have made at least one purchase (can change this to 'customers' if we want to compare all customers)
)
SELECT
    sub AS sub_status,
	CONCAT('$', FORMAT(AVG(total_spent), 2)) as avg_spent,
    COUNT(*) AS subcount,
    CONCAT(FORMAT(COUNT(*)/(SELECT COUNT(*) FROM spenders)*100, 2), '%') AS subratio
FROM subspenders
GROUP BY sub;



-- GEOGRAPHIC INFORMATION:
-- Revenue, orders, average spent per spender, number of spenders, spender share, revenue share

-- ALL countries
WITH country AS (
SELECT
    country,
    SUM(total_orders) AS country_orders,
    SUM(total_spent) AS country_rev,
    ROUND(AVG(total_spent),2) AS country_avg_spent,
    SUM(total_spent)/NULLIF((SELECT SUM(total_spent) FROM spenders),0) AS country_rev_share,
    COUNT(*) AS country_spenders,
    COUNT(*)/(SELECT COUNT(*) FROM spenders) AS country_spender_share
FROM spenders
GROUP BY country
)
SELECT *,
RANK() OVER (ORDER BY country_rev) AS country_rank
FROM country
ORDER BY country_rank DESC;

-- TOP 5 cities from each country
WITH country AS (
	SELECT
        country,
        SUM(total_spent) AS country_rev
    FROM spenders
    GROUP BY country
    ORDER BY country_rev DESC
),
city AS (
    SELECT
        s.country,
        s.province,
        s.city,
        SUM(s.total_orders) AS city_orders,
        SUM(s.total_spent) AS city_rev,
        ROUND(AVG(s.total_spent),2) AS city_avg_spent,
        SUM(s.total_spent) / NULLIF((SELECT SUM(total_spent) FROM spenders), 0) AS city_rev_share,
        COUNT(*) AS city_spenders,
        COUNT(*)/(SELECT COUNT(*) FROM spenders) AS city_spender_share
    FROM spenders s
    INNER JOIN country c ON s.country = c.country
    GROUP BY country, province, city
),
ranked AS (
    SELECT *,
           RANK() OVER (
               PARTITION BY country
               ORDER BY city_rev DESC
           ) AS country_rank
    FROM city
)
SELECT *
FROM ranked
WHERE country_rank <= 5
ORDER BY country, city_rev DESC;


-- ALL states/provinces
WITH prov AS (
SELECT
	country,
    province,
    SUM(total_orders) AS prov_orders,
    SUM(total_spent) AS prov_rev,
    ROUND(AVG(total_spent),2) AS prov_avg_spent,
    SUM(total_spent)/NULLIF((SELECT SUM(total_spent) FROM spenders),0) AS prov_rev_share,
    COUNT(*) AS prov_spenders,
    COUNT(*)/(SELECT COUNT(*) FROM spenders) AS prov_spender_share
FROM spenders
GROUP BY country, province
)
SELECT *,
RANK() OVER (ORDER BY prov_rev DESC) AS prov_rank
FROM prov
ORDER BY prov_rank ASC;


-- ALL cities
WITH city AS (
SELECT
	country,
	province,
	city,
	SUM(total_orders) AS city_orders,
	SUM(total_spent) AS city_rev,
	ROUND(AVG(total_spent),2) AS city_avg_spent,
	SUM(total_spent) / NULLIF((SELECT SUM(total_spent) FROM spenders), 0) AS city_rev_share,
	COUNT(*) AS city_spenders,
    COUNT(*)/(SELECT COUNT(*) FROM spenders) AS city_spender_share
FROM spenders
GROUP BY country, province, city
)
SELECT *,
RANK() OVER (ORDER BY city_rev DESC) AS city_rank
FROM city
ORDER BY city_rank ASC;


-- Top 5 cities from top 5 states/provinces
WITH top_prov AS (
	SELECT
        province,
        SUM(total_spent) AS prov_rev
    FROM spenders
    GROUP BY province
    ORDER BY prov_rev DESC
    LIMIT 5
),
city AS (
    SELECT
        s.country,
        s.province,
        s.city,
        SUM(s.total_orders) AS city_orders,
        SUM(s.total_spent) AS city_rev,
        ROUND(AVG(s.total_spent),2) AS city_avg_spent,
        SUM(s.total_spent) / NULLIF((SELECT SUM(total_spent) FROM spenders), 0) AS city_rev_share,
        COUNT(*) AS city_spenders,
        COUNT(*)/(SELECT COUNT(*) FROM spenders) AS city_spender_share
    FROM spenders s
    INNER JOIN top_prov t ON s.province = t.province
    GROUP BY country, province, city
),
ranked AS (
    SELECT *,
           RANK() OVER (
               PARTITION BY province
               ORDER BY city_rev DESC
           ) AS prov_rank
    FROM city
)
SELECT *
FROM ranked
WHERE prov_rank <= 5
ORDER BY province, city_rev DESC;


-- City subscribers vs. revenue
WITH ranked AS (
    SELECT
        country,
        province,
        city,
        total_spent,
        sms_opt_in,
        email_opt_in,
        ROW_NUMBER() OVER (PARTITION BY country, province, city ORDER BY total_spent) AS rn,
        COUNT(*) OVER (PARTITION BY country, province, city) AS n
    FROM spenders
),
medians AS (
    SELECT
        country,
        province,
        city,
        AVG(total_spent) AS median_spent
    FROM ranked
    WHERE rn IN (FLOOR((n + 1)/2), FLOOR((n + 2)/2))
    GROUP BY country, province, city
)
SELECT 
    s.country,
    s.province,
    s.city,
    COUNT(*) AS spender_count,
    SUM(s.total_spent)/COUNT(*) AS avg_spent,
    m.median_spent,
    SUM(s.sms_opt_in = 'yes' OR s.email_opt_in = 'yes') AS subs,
    SUM(s.sms_opt_in = 'yes' OR s.email_opt_in = 'yes') / COUNT(*) AS sub_proportion,
    SUM(s.total_spent) AS city_rev
FROM spenders s
JOIN medians m 
    ON s.country = m.country AND s.province = m.province AND s.city = m.city
GROUP BY s.country, s.province, s.city, m.median_spent
ORDER BY city_rev DESC;


-- Revenue from subscribers vs. non-subscribers by province/state
SELECT 
country,
province,
SUM(sms_opt_in = 'yes' OR email_opt_in = 'yes') AS subs, 
SUM(sms_opt_in = 'no' AND email_opt_in = 'no') AS nonsubs,
SUM(CASE 
        WHEN sms_opt_in = 'yes' OR email_opt_in = 'yes' THEN total_spent 
        ELSE 0 
    END) AS provsub_rev,
    SUM(CASE 
        WHEN sms_opt_in = 'no' AND email_opt_in = 'no' THEN total_spent 
        ELSE 0 
    END) AS provnonsub_rev,
SUM(total_spent) AS prov_rev
FROM spenders
GROUP BY country, province
ORDER BY prov_rev DESC;


-- Relevant spending information for Canada
SELECT
country,
province,
SUM(total_orders) AS prov_orders,
SUM(total_spent) AS prov_rev,
SUM(total_spent)/SUM(total_orders) AS prov_avg_spent,
COUNT(*) AS prov_spenders
FROM spenders
WHERE country = "CA"
GROUP BY country, province
ORDER BY prov_rev;
