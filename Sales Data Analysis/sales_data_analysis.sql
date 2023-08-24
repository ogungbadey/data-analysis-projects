-- SET SQL_SAFE_UPDATES = 0;
-- SET SQL_SAFE_UPDATES = 1;

-- add yr_mnt column to table, which is a concatenation of year_id and month_id
alter table practice.sales_data
add yr_mnt text;

UPDATE practice.sales_data AS main
JOIN (
    SELECT
        year_id,
        month_id,
        CONCAT(
            year_id,
            CASE
                WHEN LENGTH(month_id) = 1 THEN CONCAT('0', month_id)
                ELSE month_id
            END
        ) AS format_mnt
    FROM practice.sales_data
) AS derived_values
ON main.year_id = derived_values.year_id AND main.month_id = derived_values.month_id
SET main.yr_mnt = derived_values.format_mnt;

-- find out how many monthly active customers for each  year
select yr_mnt, count(distinct customername) as unique_customers 
from practice.sales_data
group by 
yr_mnt;

-- avg monthly sales for each year  
select year_id, month_id, round(avg(sales),2) as average_sales
from practice.sales_data 
group by year_id,month_id 
order by 1;


-- new customer growth rate
-- new customer is whoever did his/her first purchase in the time window we defined
-- i.e., Mothly in this analysis.

WITH first_purchase AS (
    SELECT
        customername,
        MIN(yr_mnt) AS first_purchase_date
    FROM practice.sales_data
    GROUP BY customername
    
),
growth_rate as 
(
	SELECT
    first_purchase_date,
    COUNT(DISTINCT customername) AS unique_customers,
    coalesce(100.0 * (COUNT(DISTINCT customername) - LAG(COUNT(DISTINCT customername)) OVER (ORDER BY first_purchase_date)) / 
    LAG(COUNT(DISTINCT customername)) OVER (ORDER BY first_purchase_date) , 0) AS percentage_change
	FROM first_purchase
	GROUP BY first_purchase_date
	ORDER BY first_purchase_date
) 
select * from growth_rate;

-- monthly revenue growth rate
with monthly_revenue as 
(
	select year_id, 
	month_id, round(sum(sales), 2) as monthly_sales
	from practice.sales_data
	group by year_id, month_id											
),
revenue_growth_rate as 
(
	select *, 
    100* (monthly_sales - lag(monthly_sales) over(order by year_id, month_id)) / 
    lag(monthly_sales) over(order by year_id, month_id)  as percentage_change 
    from monthly_revenue
)
select * from revenue_growth_rate;

-- RFM(recency, frequency monetary analysis)
with rfm as 
(
	select 
		CUSTOMERNAME, 
		sum(sales) MonetaryValue,
		avg(sales) AvgMonetaryValue,
		count(ORDERNUMBER) Frequency,
		max(ORDERDATE) last_order_date,
		(select max(ORDERDATE) from practice.sales_data) max_order_date,
		DATEDIFF((select max(ORDERDATE) from practice.sales_data),max(ORDERDATE)) Recency
	from practice.sales_data
	group by CUSTOMERNAME
),
rfm_calc as
(
	select r.*,
		NTILE(4) OVER (order by Recency desc) rfm_recency,
		NTILE(4) OVER (order by Frequency) rfm_frequency,
		NTILE(4) OVER (order by MonetaryValue) rfm_monetary
	from rfm r
)
select c.*, rfm_recency+ rfm_frequency+ rfm_monetary as rfm_cell,
concat(rfm_recency, rfm_frequency, rfm_monetary) rfm_cell_string,
case
	when rfm_recency+ rfm_frequency+ rfm_monetary >= 10 then "High Level Customer"
	when rfm_recency+ rfm_frequency+ rfm_monetary < 6 then "Low Level Customer"
	else "Mid Level Customer"
end as rfm_level
from rfm_calc c;



