## 📊 VAR and StructuralVAR | Macroeconometrics

### Monetary Policy Shocks in Emerging Economies  

Course project for **Macroeconometrics (MSc. in Economics, Universidad de San Andrés, 2025)**.  
Goal: estimate the effects of monetary policy shocks in **Chile and Mexico**, both inflation-targeting economies.  

**Data collection & processing**  
  - Domestic series: activity (IMACEC, IGAE), core inflation, nominal exchange rate, EMBI, policy rate.  
  - International variables: Fed shadow rate, U.S. inflation, global activity index, Excess Bond Premium, terms of trade.  
  - Seasonal adjustment with `seasonal` in R, log transformations.  

**Econometric modeling**  
  - Estimated **structural VARs** for each country.  
  - Lag length selection using information criteria.  
  - Recursive identification: activity and prices respond to policy shocks with at least a one-month delay.  

**Results**  
  - Impulse-response functions (IRFs) and forecast error variance decomposition. (see Graphs folder)
  - Different transmission mechanisms in Chile vs. Mexico.  
  - Monetary shocks are relevant but less important than external factors.  

---

### Tools  
We used *R*: `vars`, `seasonal`, visualization of IRFs.  
*Applied econometrics framework*, using VARs, Structural VARs, recursive identification, IRF and variance decomposition analysis.  

---

Macroeconometrics 2025 UdeSA
