TR55_Tc_qu R Project Description

The objective of this project is to analyze the effects of time of concentration (Tc) on unit peak discharge within TR-55, in particular sensitivity to changes in Tc values. The data was used from the Exhibit 4-II Unit peak discharge (qu) for NRCS (SCS) type II rainfall distribution graph in chapter 4 of the TR-55 manual, which was released in 1986. The graph is used to determine unit peak discharge (qu) from time of concentration and Ia/P (initial abstraction over precipitation) curves. qu is then used to calculate peak stormwater discharge with the equation: qp = (qu)(Am)(Q)(Fp) 

Where,
qp = peak discharge (cfs)
qu = unite peak discharge (csm/in)
Am = area (acres)
Q = runoff (in)
Fp = ponding factor 

The code-base for this project is in R, the data is in .csv format. This analysis uses the Propagate function to generate error within the time of concentration variables in relation to unit peak discharge. 
