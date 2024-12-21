capture log close
clear
clear matrix
set memory 1000m
set matsize 400
set more off


global CHEMIN="C:\Users\ruby0\Box\Personal\Year2024-25\Fall_Sep-Mar\Monday_2nd period(10.40-12.20)_Topics in Economics(Data and Econometric Methods II)\Lectures\2024.12.23_IVpaper\PrivPub\113904-V1\Archive\Result"
global CHEMIND="C:\Users\ruby0\Box\Personal\Year2024-25\Fall_Sep-Mar\Monday_2nd period(10.40-12.20)_Topics in Economics(Data and Econometric Methods II)\Lectures\2024.12.23_IVpaper\PrivPub\113904-V1\Archive"


use "$CHEMIND/dataPrivatePublic.dta", replace
 ***********************************
/* X variables definition and label*/
*************************************

gen IdF=(nregion=="116")
label var IdF "Paris_region"
gen North=(nregion=="311")
label var North "North"
gen  EconLayoff=(motins=="1")
label var EconLayoff "Economic_Layoff"
gen  PersLayoff=(motins=="2")
label var PersLayoff "Personnal_Layoff"
gen  EndCDD=(motins=="4")
label var EndCDD "End_of_Fixed_Term_Contract"
gen  EndInterim=(motins=="5")
label var EndInterim "End_of_Temporary_Work"
gen Otherend=1-EconLayoff-PersLayoff-EndCDD-EndInterim
label var Otherend "Other_reasons_of_unemployment"
gen exper0=(exper=="00")
label var exper0 "No_exp_in_the_job"
gen exper1_5=(exper=="01")+(exper=="02")+(exper=="03")+(exper=="04")+(exper=="05")
label var exper1_5 "1_to_5_years_of_exp_in_the_job"
gen experM5=1-exper0-exper1_5
label var experM5 "More_5_years_of_exp_in_the_job"
gen rsqstat2=(rsqstat=="RS2")
label var rsqstat2 "Statistical_risk_level_2"
gen rsqstat3=(rsqstat=="RS3")
label var rsqstat3 "Statistical_risk_level_3"
gen Orsqstat=1-rsqstat2-rsqstat3
label var Orsqstat "Other_Statistical_risk"
gen tempcomp=(temps=="1")
label var tempcomp "Search_for_a_full_time_position"
gen Otemp=1-(temps=="1")
label var Otemp "Do_not_search_for_a_full_time_position"
gen dezus=(zus=="ZU")
label var dezus "Sensitive_suburban_area"
gen salaireA=(salaire=="A")
label var salaireA "Wage_target_1200-1349_euros"
gen salaireB=(salaire=="B")
label var salaireB "Wage_target_1350-1549_euros"
gen salaireC=(salaire=="C")
label var salaireC "Wage_target_1550-1799_euros"
gen salaireD=(salaire=="D")
label var salaireD "Wage_target_1800-2200_euros"
gen salaireE=(salaire=="E")
label var salaireE "Wage_target_2200_euros"
gen salaireG=(salaire=="G")+(salaire=="")
label var salaireG "No_Wage_target"
gen ce1=(cemploi=="CE1")
label var ce1 "Employment_component_level_1"
gen ce2=(cemploi=="CE2")
label var ce2 "Employment_component_level_2"
gen cemiss=(cemploi=="")
label var cemiss "Employment_component_missing"

gen primo=(ndem==1)
label var primo "First_unemployment_spell"

gen Cadre=(CS==3)
label var Cadre "Manager"
gen Techn=(CS==4)
label var Techn "Technician"
gen EmployQ=(CS==51)
label var EmployQ "Skilled_clerical_worker"
gen EmployNQ=(CS==56)
label var EmployNQ "Unskilled_clerical_worker"
gen OuvrQ=(CS==61)
label var OuvrQ "Skilled_blue_colar"
gen OuvrNQ=(CS==66)+(CS==99)
label var OuvrNQ "Unskilled_blue_colar"

gen African=(1-(nation<"31"))*(nation<="49")
label var African "African"

gen EasternEurope=(1-(nation<"90"))*(nation<="98")+(nation=="24")+(nation=="25")+(nation=="27")
label var EasternEurope "Eastern_Europe"

gen SouthEuropTurkey=(nation=="02")+(nation=="03")+(nation== "14")+(nation== "19")+(nation== "21")+(nation== "22")+(nation=="24")+(nation=="27")+(nation=="26")
label var SouthEuropTurkey "South_Europe_and_Turkey"

gen nochild=(nenf==0) 
label var nochild "No_child"
gen onechild=(nenf==1)
label var onechild "One_child"
gen twoormorechild=(nenf>1)
label var twoormorechild "More_than_one_child"
gen woman=0
replace woman=1 if (sexe=="2")
label var woman "Woman"
 

label var nivetude1 "College_education"
label var nivetude1 "College_education"
label var nivetude3 "Vocational"
label var nivetude4 "High_school_dropout"
label var age3049 "Age_between_30_and_49"
label var age50p "Age_above_50"
label var femme "Woman"
label var etranger "French_citizen"
label var marie "Married"
label var motins_lic "Layoff"
label var nenf "Number_of_children"
label var agegr2635 "Aged_26_to_35"
label var agegr3645 "Aged_36_to_45"
label var agegr4655 "Aged_46_to_55"
label var agegr56 "Aged_above_56"
gen agegr26=(age<26)
label var agegr26 "age_below_26" 

gen French=etranger
label var French "French"
gen Otherregion=1-IdF-North
label var Otherregion "Other_regions"
gen Othernation=1-French-African/*-EasternEurope-SouthEuropTurkey*/
label var Othernation "Other_Nationality"


   /* Definition of variables corresponding to Region of the various operator type*/

gen TypeOPP=""
replace TypeOPP="Counseling" if lot=="6" | lot=="10" | lot=="14" | lot=="15" | lot=="16" | lot=="17"
replace TypeOPP="Interim" if lot=="12" | lot=="13" | lot=="19" | lot=="24" | lot=="25"
replace TypeOPP="Insertion" if lot=="7" | lot=="18" | lot=="22" | lot=="23"


gen conseil=(TypeOPP=="Counseling")
gen interim=(TypeOPP=="Interim")
gen insertion=(TypeOPP=="Insertion")
gen Econseil=0
gen Einterim=0
gen Einsertion=0


gen alec=ale
replace alec="77103" if ale=="77111"
replace alec="75861" if ale=="75884"
replace alec="59113" if ale=="59121"
replace alec="42024" if ale=="42002"
replace alec="42024" if ale=="42040"
replace alec="26023" if ale=="26031"


quietly tab alec,generate(alec_)


foreach  val of numlist 1/210 {
quietly sum interim if alec_`val'==1
local Ninterim=r(mean)
quietly sum insertion if alec_`val'==1
local Ninsertion=r(mean)
quietly sum conseil if alec_`val'==1
local Nconseil=r(mean)
replace Einterim=`Ninterim' if alec_`val'==1
replace Econseil=`Nconseil'  if alec_`val'==1
replace Einsertion=`Ninsertion' if alec_`val'==1
}
gen Interim=(Einterim>Econseil) & (Einterim>Einsertion)
label var Interim "Temporary_help_region"
gen Interimnc=Interim

gen Insertion=(Einsertion>Econseil) & (Einsertion>Einterim)
label var Insertion "Insertion_firm_region"
gen Insertionnc=Insertion

gen Conseil=(Econseil>Einterim) & (Econseil>Einsertion)
label var Conseil "Counseling_firm_region"
gen Conseilnc=Conseil

gen AreaTypeOPP="Interim" if Interim==1
replace AreaTypeOPP="Insertion" if Insertion==1
replace AreaTypeOPP="Counseling" if Conseil==1 


 /* cohort of assignment*/
gen Q1=inrange(mois_saisie_occ,1,3)
label var Q1 "Assigned_first_quarter" 
gen Q2=inrange(mois_saisie_occ,4,6) 
label var Q2 "Assigned_second_quarter" 
gen Q3=inrange(mois_saisie_occ,7,9)
label var Q3 "Assigned_third_quarter" 
gen Q4=inrange(mois_saisie_occ,7,9)
label var Q4 "Assigned_fourth_quarter" 


 *******************************************************
 *  Outcome variables for Cost benefit analysis        *
 *******************************************************
 
 
 
/* Cost variables*/
 
gen cost_opp=acceptationOPP*(0.3+EMPLOI_AR110_6MOIS*0.35+0.35*SUCCES_OPP_6MOIS)*3000
label var cost_opp "Cost of participation OPP if effective - accounting for performance"
gen cost_cve=acceptationCVE*657
label var cost_cve "Cost of CVE if effective"
gen cost_cla=(1-acceptationOPP-acceptationCVE)*120
label var cost_cla "Cost of standard track if effective"
gen cost = cost_opp+cost_cve+cost_cla
label var cost "Cost of counseling services: OPP cost if enter OPP, CVE cost if enter CVE, Standard track otherwise"


/* Estimation of monthly UB - Assumption that target wage is the previous wage - computation of UB according to replacement ratio*/

gen estimated_monthly_ui=0
gen pi=0.5
gen wage=0
replace wage=pi *1220+(1-pi)*1349 if salaire=="A"
replace wage=pi *1350+(1-pi)*1549 if salaire=="B"
replace wage=pi *1550+(1-pi)*1799 if salaire=="C"
replace wage=pi *1800+(1-pi)*2200 if salaire=="D"
replace wage=2600 if salaire=="E"
replace wage=. if salaire=="G"
label var wage "Estimated target wage"



/* Imputation of wage when missing based on a prediction using a large set of covariates*/
gen catsal=0
replace catsal=1 if salaire=="A"
replace catsal=2 if salaire=="B"
replace catsal=3 if salaire=="C"
replace catsal=4 if salaire=="D"
replace catsal=5 if salaire=="E"
global Xw  nivetude1 nivetude3 nivetude4 Cadre Techn EmployQ EmployNQ OuvrQ agegr2635 agegr3645 agegr4655 agegr56 woman marie nochild onechild twoormorechild French African EasternEurope SouthEuropTurkey IdF North ce1 ce2 EconLayoff PersLayoff EndCDD EndInterim exper0 exper1_5 rsqstat2 rsqstat3 tempcomp dezus primo 
ologit catsal $Xw if salaire!="G"
predict PA PB PC PD PE

replace wage=PA*(pi *1220+(1-pi)*1349)+ PB*(pi *1350+(1-pi)*1549)+PC*(pi *1550+(1-pi)*1799)+PD*(pi *1800+(1-pi)*2200)+PE*2600 if salaire=="G"
sum wage  if salaire=="G",detail
sum wage if salaire!="G", detail

/* Estimated UI using wage replacement ratio and reduced activity*/
capture drop estimated_monthly_ui
gen Replacement=0.72
replace Replacement=0.68 if wage > 1300
replace Replacement=0.64 if wage > 1900


capture drop estimated_monthly_ui
gen estimated_monthly_ui=Replacement*wage
gen estimated_ui=estimated_monthly_ui*duree_listes_horsAR/365*31
gen Total_expenses=estimated_ui+cost

sum wage estimated_monthly_ui,detail


  *******************************************************
  *          Some treatment variables                   *
  *******************************************************
 
gen acceptationCLA_6MOIS=1-acceptationCVE_6MOIS-acceptationOPP_6MOIS
gen acceptation=acceptationOPP_6MOIS if OPP==1
replace acceptation=acceptationCVE_6MOIS if CVE==1
replace acceptation=0 if OPP==0 & CVE==0


 ***********************************************************************************************

					* Propensity to enter variable employability variable

* ***********************************************************************************************

   /* List of X for the propensities and control variable*/

global X   nivetude1 nivetude3 nivetude4 Cadre Techn EmployQ EmployNQ OuvrQ agegr2635 agegr3645 agegr4655 agegr56 femme marie onechild twoormorechild French African IdF North ce1 ce2 EconLayoff PersLayoff EndCDD EndInterim exper0 exper1_5 rsqstat2 rsqstat3 tempcomp dezus salaireB salaireC salaireD salaireE salaireG primo Insertion Interim Q1 Q2 Q3
 

/*************************************************/
* mfx takes time remove * to produce the table
/*************************************************/
capture drop emp OPPprop CVEprop
logit EMPLOI_6MOIS $X [pw=POIDS_PZ_6MOIS] if SAMPLE_CVEOPP==1 & CLA==1, robust
*mfx c
predict emp
label var emp "Employability" 
test $Xb
*outreg2  using "${CHEMIN}/tableA1",mfx replace label sideway excel tex(frag) bdec(3)  nobs addstat(F, r(p)) ctitle(Employability) 

logit acceptation $X [pw=POIDS_PZ_6MOIS]  if OPP==1
*mfx c
predict OPPprop
label var OPPprop "Prop_to_enter_Priv"
*replace OPPprop=100*OPPprop
test $Xb
*outreg2  using "${CHEMIN}/tableA1",mfx append label sideway excel tex(frag) bdec(3)  nobs addstat(F, r(p)) ctitle(Entry private) 

logit acceptation $X [pw=POIDS_PZ_6MOIS] if CVE==1 
*mfx c
predict CVEprop
*replace CVEprop=100*CVEprop
label var CVEprop "Prop_to_enter_Pub"
test $Xb
*outreg2  using "${CHEMIN}/tableA1",mfx append label sideway excel tex(frag) bdec(3)  nobs addstat(F, r(p)) ctitle(Entry Public)  

gen ratioP=CVEprop/OPPprop

* ***********************************************************************************************



					* Balancing Table 2



* ***********************************************************************************************

capture prog drop pr_etoile
prog define pr_etoile
if r(p)>0.1 {
sca	etoile${e}=.
}
if r(p)<=0.1 {
sca	etoile${e}=11111
}
if r(p)<=0.05 {
sca	etoile${e}=22222
}
if r(p)<=0.01 {
sca	etoile${e}=33333
}
end


replace CLA=CLA/100
replace CVE=CVE/100
replace OPP=OPP/100


        /*  List of variable to be included in the balancing (all occurence)*/
global Xall nivetude1 nivetude2 nivetude3 nivetude4 Cadre Techn EmployQ EmployNQ OuvrQ OuvrNQ agegr26 agegr2635 agegr3645 agegr4655 agegr56 femme marie nochild onechild twoormorechild  French African  Othernation IdF North Otherregion ce1 ce2 cemiss EconLayoff PersLayoff EndCDD EndInterim Otherend exper0 exper1_5 experM5 rsqstat2 rsqstat3 Orsqstat tempcomp dezus salaireA salaireB salaireC salaireD salaireE salaireG primo Insertion Interim Conseil Q1 Q2 Q3 Q4
matrix M=.,.,.,.,.,.
foreach var in $Xall {

* mean of X by program assignment and test
/*quietly*/ reg `var' CLA CVE OPP [w=POIDSEMP_Z] if SAMPLE_CVEOPP==1, nocons r
matrix b=e(b)'
sca sCLA=b[1,1]
sca sCVE=b[2,1]
sca sOPP=b[3,1]
/*quietly*/ test OPP=CLA
global e=1
pr_etoile
/*quietly*/ test CVE=CLA
global e=2
pr_etoile
/*quietly*/ test CVE-CLA=OPP-CLA=0
global e=3
pr_etoile



local var2: var label `var'
local var2=substr("`var2'",1,32)
if "`var2'"=="" {
local var2=`var'
}

matrix N=(sCLA,sOPP,sCVE,etoile1,etoile2,etoile3)
mat rownames N="`var2'"

matrix M=M\N


}


replace CLA=CLA*100
replace CVE=CVE*100
replace OPP=OPP*100



 ********************************************************************
 *Joint tests whole sample and subsamples defined by operator region*
 ********************************************************************


matrix N=(.,.,.,.,.,.)
mat rownames N="Observations_and_joint_test"
matrix M=M\N


reg CVE $X [w=POIDSEMP_Z] if  SAMPLE_CVEOPP==1 & OPP==0
estimates store bCVE

 reg OPP $X [w=POIDSEMP_Z] if SAMPLE_CVEOPP==1 & CVE==0
estimates store bOPP

quietly suest bCVE bOPP,r


/*quietly*/ test [bOPP_mean]
sca pOPP=100*r(p)
/*quietly*/ test [bCVE_mean]
sca pCVE=100*r(p)
/*quietly*/ test ([bCVE_mean]) ([bOPP_mean])
sca pOPPCVE=100*r(p)
 
count if CLA!=0 
sca nCLA=r(N)
count if CVE!=0 
sca nCVE=r(N)
count if OPP!=0  
sca nOPP=r(N)


matrix N=(nCLA,nOPP,nCVE,pOPP,pCVE,pOPPCVE)
mat rownames N="All"
matrix M=M\N




        /* list of variable without the opertor type region variable as sample are split according to them to test balancing by the region they define*/
global Xs  nivetude1 nivetude3 nivetude4 Cadre Techn EmployQ EmployNQ OuvrQ agegr2635 agegr3645 agegr4655 agegr56 femme marie onechild twoormorechild French African IdF North ce1 ce2 EconLayoff PersLayoff EndCDD EndInterim exper0 exper1_5 rsqstat2 rsqstat3 tempcomp dezus salaireB salaireC salaireD salaireE salaireG primo Q1 Q2 Q3


foreach cond of varlist Interim Insertion Conseil {


reg CVE $Xs [w=POIDSEMP_Z] if `cond' & SAMPLE_CVEOPP==1 & OPP==0
estimates store bCVE

 reg OPP $Xs [w=POIDSEMP_Z] if `cond' & SAMPLE_CVEOPP==1 & CVE==0
estimates store bOPP

quietly suest bCVE bOPP,r


/*quietly*/ test [bOPP_mean]
sca pOPP=100*r(p)
/*quietly*/ test [bCVE_mean]
sca pCVE=100*r(p)
/*quietly*/ test ([bCVE_mean]) ([bOPP_mean])
sca pOPPCVE=100*r(p)
 
count if CLA!=0 & `cond'
sca nCLA=r(N)
count if CVE!=0 & `cond'
sca nCVE=r(N)
count if OPP!=0  & `cond'
sca nOPP=r(N)


matrix N=(nCLA,nOPP,nCVE,pOPP,pCVE,pOPPCVE)
mat rownames N="`cond'"
matrix M=M\N

}



mat colnames M=Standard Private Public (2)=(1) (3)=(1) (3)=(2)=(1) 
matrix M=M[2...,1...]
outtable using "${CHEMIN}/table2", mat(M) replace nobox format(%9.1f %9.1f %9.1f %9.0f %9.0f %9.0f) caption(Balancing test) longtable/*asis*/


* ***********************************************************************************************



					* ITT Table 3



* ***********************************************************************************************




global Ins CVE OPP



* Exit to employment
sum EMPLOI_6MOIS [aw=POIDS_PZ_6MOIS] if CLA==1
sca me=100*r(mean)
reg EMPLOI_6MOIS  $Ins [w=POIDS_PZ_6MOIS] if SAMPLE_CVEOPP==1, robust
sca NN=e(N)
local NNobs=NN


test CVE=OPP
sca pv=100*r(p)

outreg2 $Ins using "${CHEMIN}/table3", stnum(replace coef=100*coef, replace se=100*se) replace label excel tex(frag) ti("Exit before 6 months Number of observations `NNobs'") nonot ctitle(Empl) addstat(p-value private vs. public (%), pv, control mean, me ) dec(1) ad(1) nor2 addtext(Controls,  no) nocons noobs

reg EMPLOI_6MOIS  $Ins $X [w=POIDS_PZ_6MOIS] if SAMPLE_CVEOPP==1, robust

test CVE=OPP

sca pv=100*r(p)
outreg2 $Ins using "${CHEMIN}/table3", stnum(replace coef=100*coef, replace se=100*se) append label excel tex(frag)  ctitle(Empl) addstat(p-value private vs. public (%), pv, control mean, me) dec(1)  ad(1) nor2 addtext(Controls,  yes) nocons noobs


* Find eligible job
sum EMPLOI_AR110_6MOIS [aw=POIDS_PZ_6MOIS] if CLA==1
sca me=100*r(mean)
reg EMPLOI_AR110_6MOIS $Ins [w=POIDS_PZ_6MOIS] if SAMPLE_CVEOPP==1, robust

test CVE=OPP

sca pv=100*r(p)
outreg2 $Ins using "${CHEMIN}/table3", stnum(replace coef=100*coef, replace se=100*se) append label excel tex(frag) ctitle(Ext. Empl) addstat(p-value private vs. public (%), pv, control mean, me ) dec(1)  ad(1) nor2 addtext(Controls,  no) nocons noobs
reg EMPLOI_AR110_6MOIS $Ins $X [w=POIDS_PZ_6MOIS] if SAMPLE_CVEOPP==1, robust

test CVE=OPP
sca pv=100*r(p)

outreg2 $Ins using "${CHEMIN}/table3", stnum(replace coef=100*coef, replace se=100*se) append label excel tex(frag) ctitle(Ext. Empl) addstat(p-value private vs. public (%), pv, control mean, me ) dec(1)  ad(1) nor2 addtext(Controls,  yes) nocons noobs

* Find and keep eligible job

sum SUCCES_OPP_6MOIS [aw=POIDS_PZ_6MOIS] if CLA==1
sca me=100*r(mean)
reg SUCCES_OPP_6MOIS $Ins  [w=POIDS_PZ_6MOIS] if SAMPLE_CVEOPP==1, robust

test CVE=OPP
sca pv=100*r(p)

outreg2 $Ins using "${CHEMIN}/table3",stnum(replace coef=100*coef, replace se=100*se) append label excel tex(frag) ctitle(Eligible Empl) addstat(p-value private vs. public (%), pv, control mean, me ) dec(1)  ad(1) nor2 addtext(Controls,  no) nocons noobs
reg SUCCES_OPP_6MOIS $Ins $X  [w=POIDS_PZ_6MOIS] if SAMPLE_CVEOPP==1, robust

test CVE=OPP
sca pv=100*r(p)

outreg2 $Ins using "${CHEMIN}/table3", stnum(replace coef=100*coef, replace se=100*se) append label excel tex(frag) ctitle(Eligible Empl) addstat(p-value private vs. public (%), pv, control mean, me ) dec(1)  ad(1) nor2 addtext(Controls,  yes) nocons noobs


* ***********************************************************************************************



					* FIRST STAGE (table 4)



* ***********************************************************************************************



* Centering controls

foreach var in $X {
quietly sum `var' [aw=POIDS_PZ_6MOIS]
sca moy=r(mean)
replace `var'=`var'-moy
}

sum $X [aw=POIDS_PZ_6MOIS]

global Ins CVE OPP



reg acceptationCVE_6MOIS CVE OPP CLA [w=POIDS_PZ_6MOIS] if SAMPLE_CVEOPP==1, robust noc

outreg2 CVE OPP CLA using "${CHEMIN}/table4", replace stnum(replace coef=100*coef, replace se=100*se) label excel tex(frag)  dec(1)  ctitle(Enter public) nor2 nocon addtext(Controls,  No)

reg acceptationCVE_6MOIS CVE OPP CLA $X [w=POIDS_PZ_6MOIS] if SAMPLE_CVEOPP==1, robust noc

outreg2 CVE OPP CLA using "${CHEMIN}/table4", append stnum(replace coef=100*coef, replace se=100*se) label excel tex(frag)  dec(1)  ctitle(Enter public) nor2 nocon addtext(Controls,  Yes)





reg acceptationOPP_6MOIS CVE OPP CLA[w=POIDS_PZ_6MOIS] if SAMPLE_CVEOPP==1, robust noc

outreg2 CVE OPP CLA using "${CHEMIN}/table4", append stnum(replace coef=100*coef, replace se=100*se)  label excel tex(frag)  dec(1)  ctitle(Enter private) nor2 nocon addtext(Controls,  No)
reg acceptationOPP_6MOIS CVE OPP CLA $X [w=POIDS_PZ_6MOIS] if SAMPLE_CVEOPP==1, robust noc

outreg2 CVE OPP CLA using "${CHEMIN}/table4", append stnum(replace coef=100*coef, replace se=100*se) label excel tex(frag)  dec(1) ctitle(Enter private) nor2 nocon addtext(Controls,  Yes)





* ***********************************************************************************************



					* LATE (table 5)



* ***********************************************************************************************






global Expl acceptationCVE_6MOIS acceptationOPP_6MOIS

* Exit to employment
sum EMPLOI_6MOIS [aw=POIDS_PZ_6MOIS] if CLA==1
sca me=100*r(mean)

ivreg EMPLOI_6MOIS ( $Expl= CVE OPP) [w=POIDS_PZ_6MOIS] if SAMPLE_CVEOPP==1, robust

test acceptationCVE_6MOIS=acceptationOPP_6MOIS
sca NN=e(N)
local NNobs=NN
sca pv=100*r(p)
matrix b=e(b)'
sca bCVE=b[1,1]
sca bOPP=b[2,1]
sum EMPLOI_6MOIS [w=POIDS_PZ_6MOIS] if  acceptationCVE_6MOIS==1 &CVE==1
sca MCVE=r(mean)
sum EMPLOI_6MOIS [w=POIDS_PZ_6MOIS] if  acceptationOPP_6MOIS==1 & OPP==1
sca MOPP=r(mean)
sca ConterCVE=100*(MCVE-bCVE)
sca ConterOPP=100*(MOPP-bOPP)




outreg2 $Expl using "${CHEMIN}/table5", stnum(replace coef=100*coef, replace se=100*se) replace label excel tex(frag) ti("Exit before 6 months Number of observations `NNobs'") nonot ctitle(Empl) addstat(p-value private vs. public (%), pv, control mean, me,conterfactual mean pub,ConterCVE,conterfactual mean priv,ConterOPP ) dec(1) ad(1) nor2 addtext(Controls,  no) nocons noobs

ivreg EMPLOI_6MOIS $X ($Expl= CVE OPP) [w=POIDS_PZ_6MOIS] if SAMPLE_CVEOPP==1, robust
test acceptationCVE_6MOIS=acceptationOPP_6MOIS

sca pv=100*r(p)
matrix b=e(b)'
sca bCVE=b[1,1]
sca bOPP=b[2,1]
sum EMPLOI_6MOIS [w=POIDS_PZ_6MOIS] if  acceptationCVE_6MOIS==1 &CVE==1
sca MCVE=r(mean)
sum EMPLOI_6MOIS [w=POIDS_PZ_6MOIS] if  acceptationOPP_6MOIS==1 & OPP==1
sca MOPP=r(mean)
sca ConterCVE=100*(MCVE-bCVE)
sca ConterOPP=100*(MOPP-bOPP)

outreg2 $Expl using "${CHEMIN}/table5", stnum(replace coef=100*coef, replace se=100*se) append label excel tex(frag)  ctitle(Empl) addstat(p-value private vs. public (%), pv, control mean, me,conterfactual mean pub,ConterCVE,conterfactual mean priv,ConterOPP ) dec(1)  ad(1) nor2 addtext(Controls,  yes) nocons noobs


* Find eligible job
sum EMPLOI_AR110_6MOIS [aw=POIDS_PZ_6MOIS] if CLA==1
sca me=100*r(mean)
ivreg EMPLOI_AR110_6MOIS ($Expl= CVE OPP) [w=POIDS_PZ_6MOIS] if SAMPLE_CVEOPP==1, robust

test acceptationCVE_6MOIS=acceptationOPP_6MOIS

sca pv=100*r(p)
matrix b=e(b)'

sca bCVE=b[1,1]
sca bOPP=b[2,1]
sum EMPLOI_AR110_6MOIS [w=POIDS_PZ_6MOIS] if  acceptationCVE_6MOIS==1 &CVE==1
sca MCVE=r(mean)
sum EMPLOI_AR110_6MOIS [w=POIDS_PZ_6MOIS] if  acceptationOPP_6MOIS==1 & OPP==1
sca MOPP=r(mean)
sca ConterCVE=100*(MCVE-bCVE)
sca ConterOPP=100*(MOPP-bOPP)
outreg2 $Expl using "${CHEMIN}/table5", stnum(replace coef=100*coef, replace se=100*se) append label excel tex(frag) ctitle(Ext. Empl) addstat(p-value private vs. public (%), pv, control mean, me,conterfactual mean pub,ConterCVE,conterfactual mean priv,ConterOPP  ) dec(1)  ad(1) nor2 addtext(Controls,  no) nocons noobs


ivreg EMPLOI_AR110_6MOIS $X ($Expl= CVE OPP) [w=POIDS_PZ_6MOIS] if SAMPLE_CVEOPP==1, robust

test acceptationCVE_6MOIS=acceptationOPP_6MOIS

sca pv=100*r(p)
matrix b=e(b)'

sca bCVE=b[1,1]
sca bOPP=b[2,1]
sum EMPLOI_AR110_6MOIS [w=POIDS_PZ_6MOIS] if  acceptationCVE_6MOIS==1 &CVE==1
sca MCVE=r(mean)
sum EMPLOI_AR110_6MOIS [w=POIDS_PZ_6MOIS] if  acceptationOPP_6MOIS==1 & OPP==1
sca MOPP=r(mean)
sca ConterCVE=100*(MCVE-bCVE)
sca ConterOPP=100*(MOPP-bOPP)
outreg2 $Expl using "${CHEMIN}/table5", stnum(replace coef=100*coef, replace se=100*se) append label excel tex(frag) ctitle(Ext. Empl) addstat(p-value private vs. public (%), pv, control mean, me,conterfactual mean pub,ConterCVE,conterfactual mean priv,ConterOPP  ) dec(1)  ad(1) nor2 addtext(Controls,  yes) nocons noobs



* Find and keep eligible job

sum SUCCES_OPP_6MOIS [aw=POIDS_PZ_6MOIS] if CLA==1
sca me=100*r(mean)
ivreg SUCCES_OPP_6MOIS ($Expl= CVE OPP) [w=POIDS_PZ_6MOIS] if SAMPLE_CVEOPP==1, robust

test acceptationCVE_6MOIS=acceptationOPP_6MOIS

sca pv=100*r(p)

matrix b=e(b)'

sca bCVE=b[1,1]
sca bOPP=b[2,1]
sum SUCCES_OPP_6MOIS [w=POIDS_PZ_6MOIS] if  acceptationCVE_6MOIS==1 &CVE==1
sca MCVE=r(mean)
sum SUCCES_OPP_6MOIS [w=POIDS_PZ_6MOIS] if  acceptationOPP_6MOIS==1 & OPP==1
sca MOPP=r(mean)
sca ConterCVE=100*(MCVE-bCVE)
sca ConterOPP=100*(MOPP-bOPP)


outreg2 $Expl using "${CHEMIN}/table5",stnum(replace coef=100*coef, replace se=100*se) append label excel tex(frag) ctitle(Eligible Empl) addstat(p-value private vs. public (%), pv, control mean, me,conterfactual mean pub,ConterCVE,conterfactual mean priv,ConterOPP  ) dec(1)  ad(1) nor2 addtext(Controls,  no) nocons noobs
ivreg SUCCES_OPP_6MOIS $X ($Expl= CVE OPP) [w=POIDS_PZ_6MOIS] if SAMPLE_CVEOPP==1, robust

test acceptationCVE_6MOIS=acceptationOPP_6MOIS

sca pv=100*r(p)
matrix b=e(b)'

sca bCVE=b[1,1]
sca bOPP=b[2,1]
sum SUCCES_OPP_6MOIS [w=POIDS_PZ_6MOIS] if  acceptationCVE_6MOIS==1 &CVE==1
sca MCVE=r(mean)
sum SUCCES_OPP_6MOIS [w=POIDS_PZ_6MOIS] if  acceptationOPP_6MOIS==1 & OPP==1
sca MOPP=r(mean)
sca ConterCVE=100*(MCVE-bCVE)
sca ConterOPP=100*(MOPP-bOPP)


outreg2 $Expl using "${CHEMIN}/table5", stnum(replace coef=100*coef, replace se=100*se) append label excel tex(frag) ctitle(Eligible Empl) addstat(p-value private vs. public (%), pv, control mean, me,conterfactual mean pub,ConterCVE,conterfactual mean priv,ConterOPP  ) dec(1)  ad(1) nor2 addtext(Controls,  yes) nocons noobs





* ***********************************************************************************************



					* LATE Hetrogeneity wrt population of interest (table 6)



* ***********************************************************************************************



gen man=1-woman
label var man "Men"
label var woman "Women"
gen youth=(age<=29)
label var youth "less 29"
gen primeage=(1-youth)*(age<=44)
label var primeage "30-44"
gen senior=(age>=45)
label var senior "above 45"
capture drop PL NonPL
gen PL=(motins=="2")
label var PL "Pers Layoff"
gen NonPL=1-PL
label var NonPL "Other unempl" 
tab PL


global Xsplit man youth primeage senior PL NonPL





foreach d in 6 12 {
global Expl acceptationCVE_`d'MOIS acceptationOPP_`d'MOIS


sum EMPLOI_`d'MOIS [aw=POIDS_PZ_`d'MOIS] if CLA==1 & woman==1
sca me=100*r(mean)


local var2: var label woman
ivreg EMPLOI_`d'MOIS ( $Expl= CVE OPP) $X [w=POIDS_PZ_`d'MOIS] if SAMPLE_CVEOPP==1 & woman==1, robust

test acceptationCVE_`d'MOIS=acceptationOPP_`d'MOIS
sca pv=100*r(p)


outreg2 $Expl using "${CHEMIN}/table6_h`d'", stnum(replace coef=100*coef, replace se=100*se) replace label excel tex(frag) nonot ctitle(`var2') addstat(p-value private vs. public (%), pv, control mean, me ) dec(1) ad(1) nor2 nocons 

foreach var in $Xsplit {


* Exit to employment
sum EMPLOI_`d'MOIS [aw=POIDS_PZ_`d'MOIS] if CLA==1 & `var'==1
sca me=100*r(mean)
ivreg EMPLOI_`d'MOIS ( $Expl= CVE OPP) $X [w=POIDS_PZ_`d'MOIS] if SAMPLE_CVEOPP==1 & `var'==1, robust

test acceptationCVE_`d'MOIS=acceptationOPP_`d'MOIS
sca pv=100*r(p)

local var2: var label `var'
outreg2 $Expl using "${CHEMIN}/table6_h`d'", stnum(replace coef=100*coef, replace se=100*se) append label excel tex(frag) nonot ctitle(`var2') addstat(p-value private vs. public (%), pv, control mean, me ) dec(1) ad(1) nor2 nocons 

}

}






* ***********************************************************************************************



					* Figure1 LATE OLS FINAL



* ***********************************************************************************************


gen AccCVE=acceptationCVE
label var AccCVE "Enter public"
gen AccOPP=acceptationOPP
label var AccOPP "Enter private"

global Expl AccCVE AccOPP


foreach d in 3 6 9 12 {

replace AccCVE=acceptationCVE_`d'MOIS
replace AccOPP=acceptationOPP_`d'MOIS



ivreg EMPLOI_`d'MOIS $X ($Expl= CVE OPP) [w=POIDS_PZ_`d'MOIS] if SAMPLE_CVEOPP==1, robust

if `d'==3 {
outreg2 $Expl using "${CHEMIN}/Figure1.xls", stnum(replace coef=100*coef, replace se=100*se) /*sideway*/ noaster nopar replace label excel nonot ctitle(`d') noobs nocons

}

if `d'~=3 {
outreg2 $Expl using "${CHEMIN}/Figure1.xls", stnum(replace coef=100*coef, replace se=100*se) /*sideway*/ noaster nopar append label excel nonot ctitle(`d') noobs nocons

}
}


foreach d in 3 6 9 12 {

replace AccCVE=acceptationCVE_`d'MOIS
replace AccOPP=acceptationOPP_`d'MOIS



reg EMPLOI_`d'MOIS $X $Expl [w=POIDS_PZ_`d'MOIS] if SAMPLE_CVEOPP==1 , robust


outreg2 $Expl using "${CHEMIN}/Figure1.xls", stnum(replace coef=100*coef, replace se=100*se) /*sideway*/ noaster nopar append label excel nonot ctitle(`d') noobs nocons

}




**************************************************************************************
* Figure 2 & A1 Tables7 and 8
**************************************************************************************







gen empnc=emp
 *****************************************************
 * Centering Employability variable 
 *****************************************************
quietly sum emp [aw=POIDS_PZ_6MOIS] if SAMPLE_CVEOPP==1
sca memp=r(mean)
sca semp=r(sd)
gen employability=(emp-memp)/semp

quietly sum ratioP [aw=POIDS_PZ_6MOIS] if SAMPLE_CVEOPP==1
sca mratioP=r(mean)
sca sratioP=r(sd)
gen ratioPc=(ratioP-mratioP)/sratioP

gen employability2=employability^2
gen ratioPc2=ratioPc^2
gen empratioPc=employability*ratioPc

quietly sum employability2 [aw=POIDS_PZ_6MOIS] if SAMPLE_CVEOPP==1
sca memployability2=r(mean)
sca semployability2=r(sd)
replace employability2=(employability2-memployability2)/semployability2


gen AOPP=acceptationOPP_6MOIS
label var AOPP "enter private"
gen ACVE=acceptationCVE_6MOIS
label var ACVE "enter public"

foreach var of varlist employability ratioPc  employability2 ratioPc2  {
gen AOPP_`var'=AOPP*`var'
label var AOPP_`var' "enter private `var'"
gen  ACVE_`var'=ACVE*`var'
label var ACVE_`var' "enter public `var'"
gen OPP_`var'=OPP*`var'
gen CVE_`var'=CVE*`var'
}

**************************************************************************************
* Figure 2
**************************************************************************************



twoway (kdensity emp [aw=POIDSEMP_Z] if emp<0.6 & acceptationOPP_6MOIS==1) || (kdensity emp [aw=POIDSEMP_Z] if emp<0.6 & CLA==1)
twoway (kdensity emp [aw=POIDSEMP_Z] if emp<0.6 & acceptationOPP_6MOIS==1) || (kdensity emp [aw=POIDSEMP_Z] if emp<0.6 & acceptationCVE_6MOIS==1)




**************************************************************************************
* Figure A1
**************************************************************************************

sum ratioP [aw=POIDSEMP_Z], detail
kdensity ratioP [aw=POIDSEMP_Z] if ratioP<1.75



****************************************************************************************************************************************
*
*                                  Table 7
*
****************************************************************************************************************************************




matrix  C=J(12,1,0)

foreach var of varlist AOPP ACVE OPP CVE{ 


capture drop R_`var'
reg `var' $X ratioP [w=POIDS_PZ_6MOIS] if SAMPLE_CVEOPP==1
predict R_`var',r
}

foreach var of varlist EMPLOI_6MOIS EMPLOI_AR110_6MOIS SUCCES_OPP_6MOIS{ 


capture drop R_`var'
reg `var' $X ratioP [w=POIDS_PZ_6MOIS] if SAMPLE_CVEOPP==1
predict R_`var',r

capture drop R_Pi`var'
capture drop Pi`var'
gen Pi`var'=`var'*ratioP
reg Pi`var' $X ratioP [w=POIDS_PZ_6MOIS] if SAMPLE_CVEOPP==1
predict R_Pi`var',r


gmm (eq1:R_`var' - {bOPP}*R_AOPP -{bCVE}*R_ACVE - {b0}) (eq2:R_Pi`var' -  {cOPP}*R_AOPP -{cCVE}*R_ACVE - {c0})[w=POIDS_PZ_6MOIS] if SAMPLE_CVEOPP==1, instruments(R_OPP R_CVE) /*derivative(/xb1 = -1) derivative(/xb2=-1) derivative(/b1 = -1) derivative(/b2 = -1)*/ onestep winitial(unadjusted, indep)
lincom  [bOPP]_cons
sca b=r(estimate)
sca se=r(se)
matrix B=(b\se)
lincom  [cOPP]_cons
sca b=r(estimate)
sca se=r(se)
matrix B=B\(b\se)
lincom  [bCVE]_cons
sca b=r(estimate)
sca se=r(se)
matrix B=B\(b\se)
lincom  [bOPP]_cons-[bCVE]_cons
sca b=r(estimate)
sca se=r(se)
matrix B=B\(b\se)
lincom [bOPP]_cons-[cOPP]_cons
sca b=r(estimate)
sca se=r(se)
matrix B=B\(b\se)
lincom [cOPP]_cons-[bCVE]_cons
sca b=r(estimate)
sca se=r(se)
matrix B=B\(b\se)
matrix list B
matrix C=(C,B)
}
matrix D=C[1...,2..4]
matrix list D
matrix rownames D = Private_on_private_entrants sE_OPP|OPP Private_on_Publi_entrants sE_OPP|CVE Public_on_Public_entrants sE_CVE|CVE Raw_Private/Public_Difference  sRawDiff Selection_Effect sSelection Difference_on_Public_Entrants sNetDiff   
matrix colnames D = Exit Extend Eligible
matrix D=100*D
matrix list D
outtable using "${CHEMIN}/table7.tex", mat(D) replace nobox format(%9.1f %9.1f %9.1f) caption( "Selection Effect'")



****************************************************************************************************************************************
*
*                                  Table 8
*
****************************************************************************************************************************************



global explOPP AOPP  AOPP_employability AOPP_employability2 
global explCVE ACVE ACVE_employability ACVE_employability2
global expl $explOPP $explCVE
ivreg EMPLOI_6MOIS  ($expl=OPP  OPP_employability OPP_employability2 CVE CVE_employability CVE_employability2)  employability ratioPc employability2 ratioPc2 empratioPc  $X [w=POIDS_PZ_6MOIS] if SAMPLE_CVEOPP==1, robust
test  AOPP_employability AOPP_employability2 
sca pOPPa=100*r(p)
test ACVE_employability ACVE_employability2
sca pCVEa=100*r(p)
test  AOPP_employability AOPP_employability2 ACVE_employability ACVE_employability2
sca pglob=100*r(p)
test  (AOPP_employability=ACVE_employability) (AOPP_employability2=ACVE_employability2)
sca pdiffa=100*r(p)
outreg2 $explOPP using "${CHEMIN}/table8.xls", stnum(replace coef=100*coef, replace se=100*se) replace label excel /*tex(frag)*/  nonot ctitle(Exit) addstat( homgeneity private, pOPPa, homogeneity public, pCVEa , homogeneity both,pglob, same effect, pdiffa) dec(1) ad(1) nor2 addtext(Controls,  yes) nocons noobs
outreg2 $explCVE using "${CHEMIN}/table8.xls", stnum(replace coef=100*coef, replace se=100*se) append label excel /*tex(frag)*/  nonot ctitle(Exit) addstat( homgeneity private, pOPPa, homogeneity public, pCVEa , homogeneity both,pglob, same effect, pdiffa) dec(1) ad(1) nor2 addtext(Controls,  yes) nocons noobs


ivreg EMPLOI_AR110_6MOIS  ($expl=OPP  OPP_employabilit OPP_employabilit2 CVE CVE_employabilit CVE_employabilit2)  employability ratioPc employability2 ratioPc2 empratioPc  $X [w=POIDS_PZ_6MOIS] if SAMPLE_CVEOPP==1, robust
test  AOPP_employabilit AOPP_employabilit2 
sca pOPPa=100*r(p)
test ACVE_employabilit ACVE_employabilit2
sca pCVEa=100*r(p)
test  AOPP_employabilit AOPP_employabilit2 ACVE_employabilit ACVE_employabilit2
sca pglob=100*r(p)
test  (AOPP_employabilit=ACVE_employabilit) (AOPP_employabilit2=ACVE_employabilit2)
sca pdiffa=100*r(p)
outreg2 $explOPP using "${CHEMIN}/table8.xls", stnum(replace coef=100*coef, replace se=100*se) append label excel /*tex(frag)*/  nonot ctitle(Exit) addstat( homgeneity private, pOPPa, homogeneity public, pCVEa , homogeneity both,pglob, same effect, pdiffa) dec(1) ad(1) nor2 addtext(Controls,  yes) nocons noobs
outreg2 $explCVE using "${CHEMIN}/table8.xls", stnum(replace coef=100*coef, replace se=100*se) append label excel /*tex(frag)*/  nonot ctitle(Exit) addstat( homgeneity private, pOPPa, homogeneity public, pCVEa , homogeneity both,pglob, same effect, pdiffa) dec(1) ad(1) nor2 addtext(Controls,  yes) nocons noobs

ivreg SUCCES_OPP_6MOIS ($expl=OPP  OPP_employabilit OPP_employabilit2 CVE CVE_employabilit CVE_employabilit2)  employability ratioPc employability2 ratioPc2 empratioPc  $X [w=POIDS_PZ_6MOIS] if SAMPLE_CVEOPP==1, robust
test  AOPP_employabilit AOPP_employabilit2 
sca pOPPa=100*r(p)
test ACVE_employabilit ACVE_employabilit2
sca pCVEa=100*r(p)
test  AOPP_employabilit AOPP_employabilit2 ACVE_employabilit ACVE_employabilit2
sca pglob=100*r(p)
test  (AOPP_employabilit=ACVE_employabilit) (AOPP_employabilit2=ACVE_employabilit2)
sca pdiffa=100*r(p)
outreg2 $explOPP using "${CHEMIN}/table8.xls", stnum(replace coef=100*coef, replace se=100*se) append label excel /*tex(frag)*/  nonot ctitle(Exit) addstat( homgeneity private, pOPPa, homogeneity public, pCVEa , homogeneity both,pglob, same effect, pdiffa) dec(1) ad(1) nor2 addtext(Controls,  yes) nocons noobs
outreg2 $explCVE using "${CHEMIN}/table8.xls", stnum(replace coef=100*coef, replace se=100*se) append label excel /*tex(frag)*/  nonot ctitle(Exit) addstat( homgeneity private, pOPPa, homogeneity public, pCVEa , homogeneity both,pglob, same effect, pdiffa) dec(1) ad(1) nor2 addtext(Controls,  yes) nocons noobs






*****************************************************************************************************************************************
*
*              INTERIM (table 9)
*
*****************************************************************************************************************************************

gen OPPInterimnc=OPP*Interimnc
label var OPPInterimnc "Assigned to priv in a Temp area"
gen OPPInsertionnc=OPP*Insertionnc
label var OPPInsertionnc "Assigned to priv in an Insertion area"
gen OPPCounselingnc=OPP*Conseilnc
label var OPPCounselingnc "Assigned to priv in an Counseling area"

gen CVEInterimnc=CVE*Interimnc
label var CVEInterimnc "Assigned to pub in a Temp area"
gen CVEInsertionnc=CVE*Insertionnc
label var CVEInsertionnc "Assigned to pub in an Insertion area"
gen CVECounselingnc=CVE*Conseilnc
label var CVECounselingnc "Assigned to pub in an Counseling area"



gen AccOPPInterimnc=acceptationOPP_6MOIS*Interimnc
label var AccOPPInterimnc "Enter priv in a Temp area"
gen AccOPPInsertionnc=acceptationOPP_6MOIS*Insertionnc
label var AccOPPInsertionnc "Enter priv in an Insertion area"
gen AccOPPCounselingnc=acceptationOPP_6MOIS*Conseilnc
label var AccOPPCounselingnc "Enter priv in an Counseling area"


gen AccCVEInterimnc=acceptationCVE_6MOIS*Interimnc
label var AccCVEInterimnc "Enter pub in a Temp area"
gen AccCVEInsertionnc=acceptationCVE_6MOIS*Insertionnc
label var AccCVEInsertionnc "Enter pub in an Insertion area"
gen AccCVECounselingnc=acceptationCVE_6MOIS*Conseilnc
label var AccCVECounselingnc "Enter pub in an Counseling area"



gen AccPro_6MOIS=acceptationCVE_6MOIS+acceptationOPP_6MOIS
label var AccPro_6MOIS "Enter a program"
gen AccInterimnc=AccPro_6MOIS*Interimnc
label var AccInterimnc "Enter a program in a Temp area"
gen AccInsertionnc=AccPro_6MOIS*Insertionnc
label var AccInsertionnc "Enter a program in an Insertion area"
gen AccCounselingnc=AccPro_6MOIS*Conseilnc
label var AccCounselingnc "Enter a program in a Counseling area"



ivreg EMPLOI_6MOIS (AccCVEInterimnc AccCVEInsertionnc AccCVECounselingnc AccOPPInterimnc AccOPPInsertionnc AccOPPCounselingnc=CVEInterimnc CVEInsertionnc CVECounselingnc OPPInterimnc OPPInsertionnc OPPCounselingnc) Interim $X [w=POIDS_PZ_6MOIS] if SAMPLE_CVEOPP==1, robust
test AccCVEInterimnc AccCVEInsertionnc AccCVECounselingnc
sca pCVEz=100*r(p)
test AccOPPInterimnc AccOPPInsertionnc AccOPPCounselingnc
sca pOPPz=100*r(p)
outreg2 AccCVEInterimnc AccCVEInsertionnc AccCVECounselingnc AccOPPInterimnc AccOPPInsertionnc AccOPPCounselingnc using "${CHEMIN}/table9.xls", stnum(replace coef=100*coef, replace se=100*se) replace label excel /*tex(frag)*/ nonot ctitle(emp6) addstat(private , pOPPz, public,pCVEz) dec(1) ad(1) nor2 nocons 

ivreg EMPLOI_6MOIS (AccInterimnc AccInsertionnc AccCounselingnc AccOPPInterimnc AccOPPInsertionnc AccOPPCounselingnc=CVEInterimnc CVEInsertionnc CVECounselingnc OPPInterimnc OPPInsertionnc OPPCounselingnc) Interim $X [w=POIDS_PZ_6MOIS] if SAMPLE_CVEOPP==1, robust
test AccOPPInterimnc AccOPPInsertionnc AccOPPCounselingnc
sca pSamez=100*r(p)
outreg2 AccOPPInterimnc AccOPPInsertionnc AccOPPCounselingnc using "${CHEMIN}/table9.xls", stnum(replace coef=100*coef, replace se=100*se) append label excel /*tex(frag)*/ nonot ctitle(emp6) addstat(privatepublic , pSamez) dec(1) ad(1) nor2 nocons 



ivreg  EMPLOI_AR110_6MOIS (AccCVEInterimnc AccCVEInsertionnc AccCVECounselingnc AccOPPInterimnc AccOPPInsertionnc AccOPPCounselingnc=CVEInterimnc CVEInsertionnc CVECounselingnc OPPInterimnc OPPInsertionnc OPPCounselingnc) Interim $X [w=POIDS_PZ_6MOIS] if SAMPLE_CVEOPP==1, robust
test AccCVEInterimnc AccCVEInsertionnc AccCVECounselingnc
sca pCVEz=100*r(p)
test AccOPPInterimnc AccOPPInsertionnc AccOPPCounselingnc
sca pOPPz=100*r(p)
outreg2 AccCVEInterimnc AccCVEInsertionnc AccCVECounselingnc AccOPPInterimnc AccOPPInsertionnc AccOPPCounselingnc using "${CHEMIN}/table9.xls", stnum(replace coef=100*coef, replace se=100*se) append label excel /*tex(frag)*/ nonot ctitle(emp6) addstat(private , pOPPz, public,pCVEz) dec(1) ad(1) nor2 nocons 

ivreg  EMPLOI_AR110_6MOIS (AccInterimnc AccInsertionnc AccCounselingnc AccOPPInterimnc AccOPPInsertionnc AccOPPCounselingnc=CVEInterimnc CVEInsertionnc CVECounselingnc OPPInterimnc OPPInsertionnc OPPCounselingnc) Interim $X [w=POIDS_PZ_6MOIS] if SAMPLE_CVEOPP==1, robust
test AccOPPInterimnc AccOPPInsertionnc AccOPPCounselingnc
sca pSamez=100*r(p)
outreg2 AccOPPInterimnc AccOPPInsertionnc AccOPPCounselingnc using "${CHEMIN}/table9.xls", stnum(replace coef=100*coef, replace se=100*se) append label excel /*tex(frag)*/ nonot ctitle(emp6) addstat(privatepublic , pSamez) dec(1) ad(1) nor2 nocons 

ivreg  SUCCES_OPP_6MOIS (AccCVEInterimnc AccCVEInsertionnc AccCVECounselingnc AccOPPInterimnc AccOPPInsertionnc AccOPPCounselingnc=CVEInterimnc CVEInsertionnc CVECounselingnc OPPInterimnc OPPInsertionnc OPPCounselingnc) Interim $X [w=POIDS_PZ_6MOIS] if SAMPLE_CVEOPP==1, robust
test AccCVEInterimnc AccCVEInsertionnc AccCVECounselingnc
sca pCVEz=100*r(p)
test AccOPPInterimnc AccOPPInsertionnc AccOPPCounselingnc
sca pOPPz=100*r(p)
outreg2 AccCVEInterimnc AccCVEInsertionnc AccCVECounselingnc AccOPPInterimnc AccOPPInsertionnc AccOPPCounselingnc using "${CHEMIN}/table9.xls", stnum(replace coef=100*coef, replace se=100*se) append label excel /*tex(frag)*/ nonot ctitle(emp6) addstat(private , pOPPz, public,pCVEz) dec(1) ad(1) nor2 nocons 

ivreg  SUCCES_OPP_6MOIS (AccInterimnc AccInsertionnc AccCounselingnc AccOPPInterimnc AccOPPInsertionnc AccOPPCounselingnc=CVEInterimnc CVEInsertionnc CVECounselingnc OPPInterimnc OPPInsertionnc OPPCounselingnc) Interim $X [w=POIDS_PZ_6MOIS] if SAMPLE_CVEOPP==1, robust
test AccOPPInterimnc AccOPPInsertionnc AccOPPCounselingnc
sca pSamez=100*r(p)
outreg2 AccOPPInterimnc AccOPPInsertionnc AccOPPCounselingnc using "${CHEMIN}/table9.xls", stnum(replace coef=100*coef, replace se=100*se) append label excel /*tex(frag)*/ nonot ctitle(emp6) addstat(privatepublic , pSamez) dec(1) ad(1) nor2 nocons 





***********************************************************************************************************************************
*
*                                other exit table 10
*
****************************************************************************************************************************************


gen SORTIE_6MOIS=EMPLOI_6MOIS+AUTRE_6MOIS
gen AUTRERAD_6MOIS=AUTRE_6MOIS-RADIE_6MOIS

sum SORTIE_6MOIS [aw=POIDS_PZ_6MOIS] if CLA==1
sca me=100*r(mean)
ivreg SORTIE_6MOIS (acceptationCVE_6MOIS acceptationOPP_6MOIS=CVE OPP) $X [aw=POIDS_PZ_6MOIS] if SAMPLE_CVEOPP==1, robust
test acceptationCVE_6MOIS=acceptationOPP_6MOIS
sca pval=100*r(p)

outreg2 acceptationCVE_6MOIS acceptationOPP_6MOIS using "${CHEMIN}/table10_U",  stnum(replace coef=100*coef, replace se=100*se) replace label excel tex(frag) nonot ctitle( Emploi) dec(1) ad(1) addstat(P-val,pval,Control group mean,me ) nor2 nocons 


foreach var of varlist  EMPLOI_6MOIS AUTRE_6MOIS RADIE_6MOIS AUTRERAD_6MOIS{
sum `var' [aw=POIDS_PZ_6MOIS] if CLA==1
sca me=100*r(mean)
ivreg `var' (acceptationCVE_6MOIS acceptationOPP_6MOIS=CVE OPP) $X [aw=POIDS_PZ_6MOIS] if SAMPLE_CVEOPP==1, robust
test acceptationCVE_6MOIS=acceptationOPP_6MOIS
sca pval=100*r(p)
outreg2 acceptationCVE_6MOIS acceptationOPP_6MOIS  using "${CHEMIN}/table10_U",  stnum(replace coef=100*coef, replace se=100*se) append label excel tex(frag) nonot ctitle(`var') dec(1) ad(1) addstat(P-val,pval,Control group mean,me ) nor2 nocons 


}




gen SORTIE_12MOIS=EMPLOI_12MOIS+AUTRE_12MOIS
gen AUTRERAD_12MOIS=AUTRE_12MOIS-RADIE_12MOIS

sum SORTIE_12MOIS [aw=POIDS_PZ_12MOIS] if CLA==1
sca me=100*r(mean)
ivreg SORTIE_12MOIS (acceptationCVE_12MOIS acceptationOPP_12MOIS=CVE OPP) $X [aw=POIDS_PZ_12MOIS] if SAMPLE_CVEOPP==1, robust
test acceptationCVE_12MOIS=acceptationOPP_12MOIS
sca pval=100*r(p)
outreg2 acceptationCVE_12MOIS acceptationOPP_12MOIS using "${CHEMIN}/table10_L",  stnum(replace coef=100*coef, replace se=100*se) replace label excel tex(frag) nonot ctitle( Emploi) dec(1) ad(1) addstat(P-val,pval,Control group mean,me ) nor2 nocons 


foreach var of varlist  EMPLOI_12MOIS AUTRE_12MOIS RADIE_12MOIS AUTRERAD_12MOIS{
sum `var' [aw=POIDS_PZ_12MOIS] if CLA==1
sca me=100*r(mean)
ivreg `var' (acceptationCVE_12MOIS acceptationOPP_12MOIS=CVE OPP) $X [aw=POIDS_PZ_12MOIS] if SAMPLE_CVEOPP==1, robust
test acceptationCVE_12MOIS=acceptationOPP_12MOIS
sca pval=100*r(p)
outreg2 acceptationCVE_12MOIS acceptationOPP_12MOIS  using "${CHEMIN}/table10_L",  stnum(replace coef=100*coef, replace se=100*se) append label excel tex(frag) nonot ctitle(`var') dec(1) ad(1) addstat(P-val,pval,Control group mean,me ) nor2 nocons 


}









***********************************************************************************************************************************
*
*                                Cost benefit table 11
*
*
****************************************************************************************************************************************











sum cost [aw=POIDSEMP_Z] if CLA==1 & acceptationOPP==0 & acceptationCVE==0
sca me=r(mean)
reg cost CVE OPP  $X [w=POIDSEMP_Z] if SAMPLE_CVEOPP==1, robust
test CVE=OPP
sca pval=100*r(p)
outreg2 CVE OPP using "${CHEMIN}/table11_U",  replace label excel tex(frag) nonot ctitle(Cost) dec(1) ad(1) addstat(Control group mean,me ,P-val,pval) nor2 nocons 


sum duree_listes [aw=POIDSEMP_Z] if CLA==1 & acceptationOPP==0 & acceptationCVE==0
sca me=r(mean)
reg duree_listes CVE OPP $X [w=POIDSEMP_Z] if SAMPLE_CVEOPP==1, robust
test CVE=OPP
sca pval=100*r(p)
outreg2 CVE OPP using "${CHEMIN}/table11_U",  append label excel tex(frag) nonot ctitle(Days on lists) dec(1) ad(1) addstat(Control group mean,me ,P-val,pval) nor2 nocons 


sum duree_listes_horsAR [aw=POIDSEMP_Z] if CLA==1 & acceptationOPP==0 & acceptationCVE==0
sca me=r(mean)
reg duree_listes_horsAR CVE OPP $X  [w=POIDSEMP_Z] if SAMPLE_CVEOPP==1, robust
test CVE=OPP
sca pval=100*r(p)
outreg2 CVE OPP using "${CHEMIN}/table11_U",  append label excel tex(frag) nonot ctitle(Days on lists without partial employment) dec(1) ad(1) addstat(Control group mean,me ,P-val,pval) nor2 nocons 



sum estimated_ui [aw=POIDSEMP_Z] if CLA==1 & acceptationOPP==0 & acceptationCVE==0
sca me=r(mean)
reg estimated_ui CVE OPP $X [w=POIDSEMP_Z] if SAMPLE_CVEOPP==1, robust
test CVE=OPP
sca pval=100*r(p)
outreg2 CVE OPP using "${CHEMIN}/table11_U",  append label excel tex(frag) nonot ctitle(Paid UI) dec(1) ad(1) addstat(Control group mean,me ,P-val,pval) nor2 nocons 



sum Total_expenses [aw=POIDSEMP_Z] if CLA==1 & acceptationOPP==0 & acceptationCVE==0
sca me=r(mean)
reg Total_expenses CVE OPP $X [w=POIDSEMP_Z] if SAMPLE_CVEOPP==1, robust
test CVE=OPP
sca pval=100*r(p)
outreg2 CVE OPP using "${CHEMIN}/table11_U",  append label excel tex(frag) nonot ctitle(Total Expenses) dec(1) ad(1) addstat(Control group mean,me ,P-val,pval) nor2 nocons 




sum cost [aw=POIDSEMP_Z] if CLA==1 & acceptationOPP==0 & acceptationCVE==0
sca me=r(mean)
ivreg cost (acceptationCVE acceptationOPP=CVE OPP) $X [w=POIDSEMP_Z]  if SAMPLE_CVEOPP==1, robust
test acceptationCVE=acceptationOPP
sca pval=100*r(p)
outreg2 acceptationCVE acceptationOPP using "${CHEMIN}/table11_L",  replace label excel tex(frag) nonot ctitle(Cost) dec(1) ad(1) addstat(Control group mean,me,P-val,pval ) nor2 nocons 


sum duree_listes [aw=POIDSEMP_Z] if CLA==1 & acceptationOPP==0 & acceptationCVE==0
sca me=r(mean)
ivreg duree_listes (acceptationCVE acceptationOPP=CVE OPP) $X [w=POIDSEMP_Z] if SAMPLE_CVEOPP==1, robust
test acceptationCVE=acceptationOPP
sca pval=100*r(p)
outreg2 acceptationCVE acceptationOPP using "${CHEMIN}/table11_L",  append label excel tex(frag) nonot ctitle( Days on lists) dec(1) ad(1) addstat(Control group mean,me,P-val,pval ) nor2 nocons 

 
sum duree_listes [aw=POIDSEMP_Z] if CLA==1 & acceptationOPP==0 & acceptationCVE==0
sca me=r(mean)
ivreg duree_listes_horsAR (acceptationCVE acceptationOPP=CVE OPP) $X [w=POIDS_PZ_6MOIS] if SAMPLE_CVEOPP==1, robust
test acceptationCVE=acceptationOPP
sca pval=100*r(p)
outreg2 acceptationCVE acceptationOPP using "${CHEMIN}/table11_L",  append label excel tex(frag) nonot ctitle( Days on lists without partial employment) dec(1) ad(1) addstat(Control group mean,me,P-val,pval ) nor2 nocons 


sum estimated_ui [aw=POIDSEMP_Z] if CLA==1 & acceptationOPP==0 & acceptationCVE==0
sca me=r(mean)
ivreg estimated_ui (acceptationCVE acceptationOPP=CVE OPP) $X [w=POIDSEMP_Z] if SAMPLE_CVEOPP==1, robust
test acceptationCVE=acceptationOPP
sca pval=100*r(p)
outreg2 acceptationCVE acceptationOPP using "${CHEMIN}/table11_L",  append label excel tex(frag) nonot ctitle( Paid UI) dec(1) ad(1) addstat(Control group mean,me,P-val,pval ) nor2 nocons 


sum Total_expenses [aw=POIDSEMP_Z] if CLA==1 & acceptationOPP==0 & acceptationCVE==0
sca me=r(mean)
ivreg Total_expenses (acceptationCVE acceptationOPP=CVE OPP) $X [w=POIDSEMP_Z] if SAMPLE_CVEOPP==1, robust
test acceptationCVE=acceptationOPP
sca pval=100*r(p)
outreg2 acceptationCVE acceptationOPP using "${CHEMIN}/table11_L",  append label excel tex(frag) nonot ctitle( Total expenses) dec(1) ad(1) addstat(Control group mean,me,P-val,pval ) nor2 nocons 

