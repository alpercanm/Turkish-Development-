The research file contains data from TurkStat 
Sheets
Sheet	Rows	Units
all	2,050	81 provinces + Türkiye total (82 units × 25 years)
treat	1,800	71 provinces + 1 aggregated treatment unit (72 units × 25 years)
Identifiers
Column	Description
NUTS-3 code (nuts3_code in treat)	NUTS-3 region code (e.g. TR100 = İstanbul). TR = national total
plate	Province licence-plate code (1–81). 82 = Türkiye total, 100 = treatment unit
province	Province name (Turkish spelling)
year	Calendar year, 2000–2024
Sector columns (NACE Rev.2 sections)

Gross value added by activity, in thousand US dollars at current prices.

Column	NACE Rev.2 section(s)
A	Agriculture, forestry and fishing
BCDE	Industry: mining and quarrying (B); manufacturing (C); electricity, gas, steam and air conditioning supply (D); water supply, sewerage, waste management and remediation (E)
C	Manufacturing — a sub-component of BCDE, not an additional sector
F	Construction
GHI	Wholesale and retail trade, repair of motor vehicles (G); transportation and storage (H); accommodation and food service activities (I)
J	Information and communication
K	Financial and insurance activities
L	Real estate activities
MN	Professional, scientific and technical activities (M); administrative and support service activities (N)
OPQ	Public administration and defence, compulsory social security (O); education (P); human health and social work activities (Q)
RST	Arts, entertainment and recreation (R); other service activities (S); activities of households as employers (T)
Other variables
Column	Description
Sectoral total	Total gross value added at basic prices (thousand USD)
Taxes-subsidies	Taxes less subsidies on products (thousand USD)
GDP	Gross domestic product (thousand USD)
population	Population (persons)
TL/USD rate (tl_usd_rate in treat)	Turkish lira per US dollar, annual; one value per year, identical across units
pop_den	Population density (persons per km²)
median_age	Median age (years)
male_female_ratio	Males per female
Accounting identities

These hold exactly in every row:

Sectoral total = A + BCDE + F + GHI + J + K + L + MN + OPQ + RST
GDP            = Sectoral total + Taxes-subsidies

C is excluded from the sum because it is already contained in BCDE. In the all sheet, the 81 provinces sum to the Türkiye row for GDP, all sector columns and population.

The treat sheet

The treat sheet removes ten provinces from the panel and replaces them with a single aggregated unit (province = treatment, plate = 100, nuts3_code = tr999). The ten provinces are:

Ağrı (4), Bitlis (13), Diyarbakır (21), Hakkari (30), Muş (49), Tunceli (62), Mardin (47), Van (65), Batman (72), Şırnak (73)

The remaining 71 provinces are unchanged from all. The Türkiye total row is not included.

How the treatment unit is built:

Sector values, Sectoral total, Taxes-subsidies, GDP and population are sums over the ten provinces.
pop_den is total population divided by total land area of the ten provinces (not an average of the provincial densities).
median_age is the population-weighted mean of the provincial medians.
male_female_ratio is recomputed for the combined region.

Additional per-capita columns in treat:

Column	Description
A_pc, BCDE_pc, C_pc, F__pc, GHI_pc, J_pc, K__pc, L__pc, MN__pc, OPQ_pc, RST_pc	Sector value added per person (USD)
sectoral_total_pc	Gross value added per person (USD)
Taxes-subsidies__pc	Taxes less subsidies per person (USD)
gdp_pc	GDP per person (USD) = GDP × 1000 / population
gdp_pc_tl	GDP per person in Turkish lira = gdp_pc × tl_usd_rate
