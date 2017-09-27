

--------------------------------------- POPULATION CLASSES ------------------------------------------
IF OBJECT_ID('tempdb..#pop_classes') IS NOT NULL
    DROP TABLE #pop_classes
select *
into #pop_classes
from (
	select s.*
		,coalesce([Initial Grade],[Final Grade]) Grade	-- prioritizing initial grade because these are historic data and I want the grade that was given as soon as the course finished.
		,case when coalesce([Initial Grade],[Final Grade]) = 'A' then 0
			when coalesce([Initial Grade],[Final Grade]) = 'A-' then 1
			when coalesce([Initial Grade],[Final Grade]) = 'B+' then 2
			when coalesce([Initial Grade],[Final Grade]) = 'B' then 3
			when coalesce([Initial Grade],[Final Grade]) = 'B-' then 4
			when coalesce([Initial Grade],[Final Grade]) = 'C+' then 5
			when coalesce([Initial Grade],[Final Grade]) = 'C' then 6
			when coalesce([Initial Grade],[Final Grade]) = 'C-' then 7
			when coalesce([Initial Grade],[Final Grade]) = 'D+' then 8
			when coalesce([Initial Grade],[Final Grade]) = 'D' then 9
			when coalesce([Initial Grade],[Final Grade]) = 'D-' then 10
			when coalesce([Initial Grade],[Final Grade]) = 'F' then 11
			when coalesce([Initial Grade],[Final Grade]) = 'AU' then 0
			when coalesce([Initial Grade],[Final Grade]) = 'H' then 0
			when coalesce([Initial Grade],[Final Grade]) = 'S' then 3
			when coalesce([Initial Grade],[Final Grade]) = 'U' then 11
			when coalesce([Initial Grade],[Final Grade]) = 'I' then 11
			when coalesce([Initial Grade],[Final Grade]) = 'W' then 11
			when coalesce([Initial Grade],[Final Grade]) = 'IP' then 11 --none should exist in this data set but will exist in the future. considering a poor outcome since the student didn't complete the course in time.
			end as GrOrd	--this column is needed later
	from IR.dbo.vw_SCAR s
	join (select [Student ID],min([Completion Date]) as CmpDt_Min
				,min([First Course Date]) as FCD_Min
			from IR.dbo.vw_SCAR
			where Units > 0 --this excludes ORI classes for all and BUS 500A for GRAD students
				and [Completed Course] = 1
			group by [Student ID]) m
		on s.[Student ID] = m.[Student ID]
		and s.[Completion Date] = m.CmpDt_Min	--dont join on FCD - want all the classes if they ended on the same date
		and s.[Completed Course] = 1
	where Units > 0
		and s.[Degree Award Type] in ('Associate Degree','Bachelor Degree','Master''s Degree')
		and s.Term between 1407 and 1606
) t


--------------------------------------- POPULATION DISTINCT ------------------------------------------
IF OBJECT_ID('tempdb..#pop') IS NOT NULL
    DROP TABLE #pop
select ROW_NUMBER () over(order by StuID,FCD_Min,CmpDt_Min) as RowID
	,*
into #pop
from (
	select [Student ID] as StuID,Career,[Degree Award Type],[Entity Group],[Academic Plan]
		,[Plan Description],[Acad Sub Plan],[Sub Plan Description]
		,min([Class Enroll Date]) as EnrlDt_Min
		,min([First Course Date]) as FCD_Min
		,min([Completion Date]) as CmpDt_Min
		,min(Term) as Term_Min
		,min([Fiscal Year]) as FY
	from #pop_classes
	group by [Student ID],Career,[Degree Award Type],[Entity Group],[Academic Plan]
	,[Plan Description],[Acad Sub Plan],[Sub Plan Description]
) d





--------------------------------------- OUTCOME MEASURE ------------------------------------------
IF OBJECT_ID('tempdb..#outcome') IS NOT NULL
    DROP TABLE #outcome
select *,case when Retained = 'N' and Graduated = 'N' then 'Y' else 'N' end as Dropout
into #outcome
from (
	select p.RowID, p.StuID
		,case when o.[Student ID] is not null then 'Y' else 'N' end as Retained
		,case when p.FY = a.GradFY then 'Y' 
			 when p.FY + 1 = a.GradFY then 'Y' 
			 else 'N' end as Graduated
	from #pop p
	left join (select distinct [Student ID],Career,[Fiscal Year]
				from IR.dbo.vw_SCAR s
				where Term >= 1507 and [Completed Course] = 1
					and s.[Degree Award Type] in ('Associate Degree','Bachelor Degree','Master''s Degree')) o
		on p.StuID = o.[Student ID]
		and p.Career = o.Career
		and p.FY + 1 = o.[Fiscal Year]
	left join (select distinct EMPLID, ACAD_CAREER, DEGREE, ACAD_PLAN, ACAD_SUB_PLAN_1,cast([CONFER-COMP_DATE] as date) as GradDT
					,case when month([CONFER-COMP_DATE]) >=7 then year([CONFER-COMP_DATE]) + 1
						else year([CONFER-COMP_DATE])
						end as GradFY
				from IR.dbo.Alum12 a
				where ACAD_PLAN_TYPE = 'MAJ') a
		on p.StuID = a.EMPLID
		and p.Career = a.ACAD_CAREER
		and p.FY >= a.GradFY
	) m







--------------------------------------- BIO Demo ------------------------------------------
IF OBJECT_ID('tempdb..#demo') IS NOT NULL
    DROP TABLE #demo
select *
into #demo
from (
	select p.RowID
		,p.StuID
		,case when b.EMPLID is not null then 1 else 0 end as BioDataExists
		,b.[First Name], b.[Middle Name], b.[Last Name], b.[Pref Email], b.[Pref Phone]
		,isnull(b.Sex,'U') as Gender
		,case when b.Ethnicity is null then 'Unknown'
			when b.Ethnicity = 'Elected not to respond' then 'Unknown'
			else b.Ethnicity end as Ethnicity
		,DATEDIFF(year,b.DOB,p.FCD_min) as Age	-- USE AGE GROUP INSTEAD?
		,case when DATEDIFF(year,b.DOB,p.FCD_min) <= 24 then '<25'
			when DATEDIFF(year,b.DOB,p.FCD_min) <= 29 then '25-29'
			when DATEDIFF(year,b.DOB,p.FCD_min) <= 39 then '30-39'
			when DATEDIFF(year,b.DOB,p.FCD_min) >= 40 then '40+' end as AgeCat
		,case when b.[Military Status] in ('Active Duty','Dep of Active Duty') then 'Y' else 'N' end as ActiveDuty
		,case when b.[Military Status] in (select MStatus from IRDev.xw.vw_MilitaryStatus where [Military Y/N] = 'Y') then 'Y' else 'N' end MilitaryYN
		,isnull(b.[Military Status],'Not indicated') as MilitaryStatus
		,coalesce(AGI_PerCap_Zip,AGI_PerCap_ZipPre,AGI_PerCap_ST
			,(select cast(sum(A00100)*1000 / (sum(mars1) + sum(mars2)*2 + sum(mars4) + sum(NUMDEP)) as int) AGI_PerCap_US from IRDev.irs.SOI_2014)) AGI_PerCapita
		,coalesce(AGI_zip,AGI_ZipPre,AGI_State,'4 US') AGI_Source
		,j.CIP2D
		,case when pr.EMPLID is not null then 'Y' else 'N' end as Probation
	from #pop p
	left join	--get bio demo and reformat Zipcodes
		(select *
			,case when isnumeric(left([Home Zip],5))=1 then cast(left([Home Zip],5) as int) else null end as HomeZip
			,case when isnumeric(left([Home Zip],5))=1 then left(right('00000' + rtrim(cast(cast(left([Home Zip],5) as int) as char)),5),3) else null end as HomeZipPref
		 from IR.dbo.BioDemo) b
		on p.StuID = b.EMPLID
	left join	--AGI for Zipcodes
		(select '1 Zip' as AGI_zip,zipcode,cast(sum(A00100)*1000 / (sum(mars1) + sum(mars2)*2 + sum(mars4) + sum(NUMDEP)) as int) AGI_PerCap_Zip 
		 from IRDev.irs.SOI_2014 s where zipcode not in (0,99999) group by STATEFIPS,[STATE],zipcode) z
		on b.HomeZip = z.zipcode 
	left join	--AGI for Zip Prefixes (3 digits)
		(select '2 ZipPrefix' as AGI_ZipPre,left(right('00000' + rtrim(cast(zipcode as char)),5),3) as ZipPrefix
			,cast(sum(A00100)*1000 / (sum(mars1) + sum(mars2)*2 + sum(mars4) + sum(NUMDEP)) as int) AGI_PerCap_ZipPre 
		 from IRDev.irs.SOI_2014 s where zipcode not in (0,99999) group by left(right('00000' + rtrim(cast(zipcode as char)),5),3)) zp
		on b.HomeZipPref = zp.ZipPrefix
	left join	--AGI for State
		(select '3 State' as AGI_State,[STATE],cast(sum(A00100)*1000 / (sum(mars1) + sum(mars2)*2 + sum(mars4) + sum(NUMDEP)) as int) AGI_PerCap_ST 
		 from IRDev.irs.SOI_2014 s where zipcode not in (0,99999) group by [STATE]) zs
		on b.[Home St] = zs.[STATE]
	left join (select distinct [Acad Plan],[CIP Code],c.[2D_CIPFamilyCode] + ' - ' + c.[2D_CIPTitle] as CIP2D 
				from IR.soar.NU_IE_ACAD_PLAN_SETUP n
				left join IR.dbo.CIPCode2010_Hierarchical c
					on n.[CIP Code] = c.[6D_CIPCode]) j
		on p.[Academic Plan] = j.[Acad Plan]
	left join ir.dbo.PRO pr	--enter on probation
		on p.StuID = pr.EMPLID
		and p.Career = pr.ACAD_CAREER
		and p.Term_Min >= pr.EntryTerm
	) b



---------------------------------------------------- AAR For Core Or Elective Class ------------------------------------------------
IF OBJECT_ID('tempdb..#aar') IS NOT NULL
    DROP TABLE #aar
select *
into #aar
from (select distinct ACAD_PLAN1, CRSE_ID, 100 as CPE
		from IR..AAR
		where DESCRSHORT1 in ('CORE','PREP','ELEC')
			and CRSE_ID is not null
			and CRSE_ID <>''
			and DESCR1 not like '%Minor%') a



---------- Students in Class (SIC) -----------
-- Get total enrollment for each class
IF OBJECT_ID('tempdb..#ClassSIC') IS NOT NULL
    DROP TABLE #ClassSIC
select *
into #ClassSIC
from (
select s.Term,s.[Course ID],s.[Class Nbr],s.[Subject],s.[Catalog],s.[Course Title]
	,count(*) SIC
	from (
		select distinct Term,[Course ID],[Class Nbr] 
		from #pop_classes) c
	left join ir.dbo.vw_SCAR s
		on c.Term = s.Term
		and c.[Course ID] = s.[Course ID]
		and c.[Class Nbr] = s.[Class Nbr]
	group by s.Term,s.[Course ID],s.[Class Nbr],s.[Subject],s.[Catalog],s.[Course Title]
	) c



---------- Class Grades for All Students -----------
--Then get every student that enrolled in those classes
IF OBJECT_ID('tempdb..#ClassGrades') IS NOT NULL
    DROP TABLE #ClassGrades
select *
into #ClassGrades
from (
select ROW_NUMBER () over (partition by Term,[Course ID],[Class Nbr] order by GrOrd) rn
	,*
from (
	select coalesce([Initial Grade],[Final Grade]) Grade
		,case when coalesce([Initial Grade],[Final Grade]) = 'A' then 0
			when coalesce([Initial Grade],[Final Grade]) = 'A-' then 1
			when coalesce([Initial Grade],[Final Grade]) = 'B+' then 2
			when coalesce([Initial Grade],[Final Grade]) = 'B' then 3
			when coalesce([Initial Grade],[Final Grade]) = 'B-' then 4
			when coalesce([Initial Grade],[Final Grade]) = 'C+' then 5
			when coalesce([Initial Grade],[Final Grade]) = 'C' then 6
			when coalesce([Initial Grade],[Final Grade]) = 'C-' then 7
			when coalesce([Initial Grade],[Final Grade]) = 'D+' then 8
			when coalesce([Initial Grade],[Final Grade]) = 'D' then 9
			when coalesce([Initial Grade],[Final Grade]) = 'D-' then 10
			when coalesce([Initial Grade],[Final Grade]) = 'F' then 11
			when coalesce([Initial Grade],[Final Grade]) = 'AU' then 0
			when coalesce([Initial Grade],[Final Grade]) = 'H' then 0
			when coalesce([Initial Grade],[Final Grade]) = 'S' then 3
			when coalesce([Initial Grade],[Final Grade]) = 'U' then 11
			when coalesce([Initial Grade],[Final Grade]) = 'I' then 11
			when coalesce([Initial Grade],[Final Grade]) = 'W' then 11
			when coalesce([Initial Grade],[Final Grade]) = 'IP' then 11 --none should exist in this data set but will exist in the future. considering a poor outcome since the student didn't complete the course in time.
			when coalesce([Initial Grade],[Final Grade]) is null then 0	-- these are non-graded orientation or activity fee classes 
			end as GrOrd
		,c.SIC
		,s.*
	from #ClassSIC c
	left join ir.dbo.vw_SCAR s
		on c.Term = s.Term
		and c.[Course ID] = s.[Course ID]
		and c.[Class Nbr] = s.[Class Nbr]
	) c
) c



---------- Class Median Grade -----------
IF OBJECT_ID('tempdb..#ClassMedian') IS NOT NULL
    DROP TABLE #ClassMedian
select *
into #ClassMedian
from (
	select Median,Grade,GrOrd,SIC,Term,[Course ID],[Class Nbr],[Subject],[Catalog],[Course Title]
	from (
	select case when SIC = 1 then 'Median'
		when SIC/2 = rn then 'Median' end as Median
		,*
	from #ClassGrades
	) c 
	where Median is not null
)c






---------- End Of Course Evaluations -----------
IF OBJECT_ID('tempdb..#EndEval') IS NOT NULL
    DROP TABLE #EndEval
select *
into #EndEval
from (
	select *	
	from (
		select [Stu ID],Term,[Class Nbr],[First Class Date],[Last Class Date],[Perception of]
			,case when [Level] = 'U' then 'UGRD' when [Level] = 'G' then 'GRAD' when [Level] = 'E' then 'EXED' end Career
			,AVG(cast(nullif(Response,6) as numeric)) A -- "NA" = 6 so exclude these
		from IRDev.dbo.vw_EndOfCourseEval_t
		group by [Stu ID],Term,[Class Nbr],[First Class Date],[Last Class Date],[Perception of]
			,case when [Level] = 'U' then 'UGRD' when [Level] = 'G' then 'GRAD' when [Level] = 'E' then 'EXED' end
		having count(Response) > (case when [Perception of] = 'Learning' then 4 else 6 end) --needed to answer more than half of the questions ("NA"/"6" is a valid response) (Learning has 8 and Teaching has 12)
		) p
	PIVOT (avg(A) for [Perception of] in ([Learning],[Teaching])) as PivTable
	) e




---------- FIRST CLASS -----------
-- Aggregated class data for analysis cohort
IF OBJECT_ID('tempdb..#FirstClass') IS NOT NULL
    DROP TABLE #FirstClass
select *
into #FirstClass
from (
	select RowID
		,StuID
		,Career
		,avg(DATEDIFF(day,[First Course Date],[Completion Date])) as ClassLength_avg
		,count(*) as Classes
		,sum(Units) as Units
		,case when sum(DFUWI) > 0 then 'Y' else 'N' end DFUWI  --flagging any DFUWI
		,case when sum(WW) = count(*) then 'Y' else 'N' end as OnlineOnly
		,case when sum(Adjunct) = count(*) then 'Y' else 'N' end as AdjunctOnly
		,case when sum(RemedialCrs) > 0 then 'Y' else 'N' end as RemedialCrs --flagging any RemedialCrs
		,case when avg(Learning) > 3 then 'Positive' when avg(Learning) <= 3 then 'Negative' else 'No Response' end as LearningGrp
		,case when avg(Teaching) > 3 then 'Positive' when avg(Teaching) <= 3 then 'Negative' else 'No Response' end as TeachingGrp
		,case when sum(UnitWeight*RelativePerf) > 0.5 then 'Above Median' else 'Below Median' end as RelativePerf
		,avg(SIC) as SIC_avg
		,case when avg(UnitWeight*CPE) >= 0.5 then 'Y' else 'N' end ClassCPE  --values are either 100% or 0% so collapsing to BIT
	from (
		select p.RowID, p.StuID
			,s.TotalUnits
			,c.Units / s.TotalUnits as UnitWeight
			,case when coalesce([Initial Grade],[Final Grade]) in ('D+','D','D-','F','U','W','I','IP') then 100 else 0 end DFUWI	--need to use Initial Grade since this is what will be used in production. sometimes final grade is populated without initial being populated
			,case when c.[Instruction Mode] = 'WW' then 1 else 0 end as WW
			,case when c.[Job Type] in ('Adjunct') then 1 else 0 end as Adjunct
			,case when c.subject + c.catalog in ('ENG13','MTH12A','MTH12B') then 1 else 0 end as RemedialCrs
			,e.Learning
			,e.Teaching
			,case when c.GrOrd <= m.GrOrd then 100	--'At or Above Median'
				when c.GrOrd > m.GrOrd then 0		--'Below Median' 
				end as RelativePerf
			,m.SIC
			,a.CPE
			,c.*
		from #pop p
		left join #pop_classes c
			on p.StuID = c.[Student ID]
			and p.Career = c.Career
		left join (select [Student ID],Career,sum(Units) as TotalUnits from #pop_classes group by [Student ID],Career) s	--get total units for tie-breakers
			on p.StuID = s.[Student ID]
			and p.Career = s.Career
		left join #EndEval e
			on c.[Student ID] = e.[Stu ID]
			and c.Term = e.Term
			and c.[Class Nbr] = e.[Class Nbr]
			and c.Career = e.Career
			and c.[First Course Date] = e.[First Class Date]
			and c.[Completion Date] = e.[Last Class Date]
		left join #ClassMedian m
			on c.Term = m.Term
			and c.[Course ID] = m.[Course ID]
			and c.[Class Nbr] = m.[Class Nbr]
		left join #aar a
			on c.[Academic Plan] = a.ACAD_PLAN1
			and c.[Course ID] = a.CRSE_ID
		) a
	group by RowID,StuID,Career
) a




-------------------------------------- SCHEDULE -----------------------------------------
IF OBJECT_ID('tempdb..#Schedule') IS NOT NULL
    DROP TABLE #Schedule
select *
into #Schedule
from (
	select p.RowID
		,p.StuID
		,p.Career
		,p.Term_Min
		,p.FY
		,p.EnrlDt_Min
		,p.FCD_Min
		,p.CmpDt_Min
		,max(s.[First Course Date]) as MaxSchldFCD
		,max(s.[Class Enroll Date]) as MaxSchldEnrlDt
		,DATEDIFF(day,p.FCD_min,cast(cast(p.FY as char(4)) + '-07-01' as date)) DaysToFYClose
		,case when month(p.FCD_min) between 7 and 9 then 'Q1'
			when month(p.FCD_min) between 10 and 12 then 'Q2'
			when month(p.FCD_min) between 1 and 3 then 'Q3'
			when month(p.FCD_min) between 4 and 6 then 'Q4'
			end as FCD_FYQ
		,DATEDIFF(day,p.EnrlDt_Min,p.FCD_Min) DaysToFC
	from #pop p
	left join IR.dbo.vw_SCAR s
		on p.StuID = s.[Student ID]
		and p.Career = s.Career
	group by p.RowID
		,p.StuID,p.Career,p.Term_Min,p.EnrlDt_Min
		,p.FCD_Min,p.CmpDt_Min,p.FY
	) s




---------------------- SERVICE INDICATORS -------------------------
-- Create Working Service Indicator Table
IF OBJECT_ID('tempdb..#ServInd_work') IS NOT NULL
    DROP TABLE #ServInd_work
select *
into #ServInd_work
from (
	select RowID,StuID,Career
		,SI_Stamp,SI_Code,SI_Desc,SI_Reason,SI_Reason_Desc,SI_Positive,SI_Positive_Bit
		,sum(Audit_Act_Value) AAV
	from #pop p
	left join IRDev.dbo.vw_NU_IR_SERVICE_INDICATOR_AUDITS a
		on p.StuID = a.ID
		and a.Audit_Stamp <= p.CmpDt_Min	--SI must be closed by last class date
	group by RowID,StuID,Career
		,ID,SI_Stamp,SI_Code,SI_Desc,SI_Reason,SI_Reason_Desc,SI_Positive,SI_Positive_Bit
	having sum(Audit_Act_Value) > 0	--leaves only open service indicators
) ServInd




-- CREATE WIDE TABLE TO JOIN TO ANALYSIS TABLE
IF OBJECT_ID('tempdb..#ServInd') IS NOT NULL
    DROP TABLE #ServInd
select *
into #ServInd
from (
	select *	--distinct SI_Group
	from (
	select RowID,StuID,Career
		,case when SI_Positive = 'Y' then 'P' else SI_Positive end + '_' +
			case when SI_Code in ('BCB','BCR','BCW','BKA','BLK','BOM','BPP','BRR','BSP','BSS','BV3'
								,'BVA','BVO','CRD','ECD','ECP','ECS','EPS'
								,'FAC','FAO','FAS','FBA','FPW','FRD','HIC','NOS','REC','REV','RFA','ROR') then SI_Code
				when SI_Code in ('BCH','BCS','BLH','RTR','SA1') then 'Hold'
				when SI_Code in ('BDT','BTP','BTA','EVA','EVN','VSC') then 'VaTuit'
				when SI_Code in ('RSC','RSD','RSE') then 'SOC'
				when SI_Code in ('INT','IOS') then 'IntlStu'
				when SI_Code in ('ATN','FHB','FNG','ICM','S2S') then 'MiscSchlr'
				when SI_Code in ('BPW','BUW','BWO','BWR') then 'WriteOff'
				when SI_Desc like '%B2B%' then 'B2B'
				when SI_Desc like '%offsite cohort%' then 'OffsiteDisc'
				end as SI_Group
		,'Y' as S
	from #ServInd_work s
	) p
	PIVOT (min(S) for SI_Group in (N_BCW,	N_BKA,	N_BLK,	N_BPP,	N_CRD,	N_ECD,	N_FPW,	N_Hold,	N_IntlStu,	
		N_REC,	N_WriteOff,	P_B2B,	P_BCB,	P_BCR,	P_BOM,	P_BRR,	P_BSP,	P_BSS,	P_BV3,	P_BVA,	P_BVO,	
		P_ECP,	P_ECS,	P_EPS,	P_FAC,	P_FAO,	P_FAS,	P_FBA,	P_FRD,	P_HIC,	P_MiscSchlr,	P_NOS,	
		P_OffsiteDisc,	P_REV,	P_RFA,	P_ROR,	P_SOC,	P_VaTuit)) as PivTable
) si







-------------------------------------------- FAFSA -----------------------------------------------
IF OBJECT_ID('tempdb..#FAFSA') IS NOT NULL
    DROP TABLE #FAFSA
select distinct 
	p.RowID,p.StuID,p.Career
	,case when p.FCD_Min >= F.[Eff Date] then 'BeginFirstClass'
		when p.CmpDt_Min >= F.[Eff Date] then 'EndFirstClass'
		else 'NoFAFSA' end as FAFSAby
	,case when f.[PAR Fa Grd/Lvl] = 'College' or f.[PAR Mo Grd/Lvl] = 'College' then 'College'
		when f.[PAR Fa Grd/Lvl] = 'High School' or f.[PAR Mo Grd/Lvl] = 'High School' then 'High School'
		when f.[PAR Fa Grd/Lvl] = 'Middle School' or f.[PAR Mo Grd/Lvl] = 'Middle School' then 'Middle School'
		else 'Unknown' end as ParentHiEd
	,f.AGI	
	,case when f.[Food Stamps] = 'Yes' or f.[School Lunch] = 'Yes' or f.SSI = 'Yes' or f.TANF = 'Yes' or f.WIC = 'Yes' then 'Y'
		else 'N' end as GovtPrgmY
	,case when f.Dependents = 'Yes' then 'Y' else 'N' end as DependentsY			
	,case when f.Children = 'Yes' then 'Y' else 'N' end as ChildrenY
	,f.[In Family]
	,isnull(f.[Mar Stat],'Unknown') as MaritalStat
	,coalesce(f.[Prorated EFC],f.[Prmry EFC]) as EFC 
into #FAFSA
from #pop p
left join IR.dbo.FAFSA f
	on p.StuID = f.ID
	and p.Career = f.Career
	and p.CmpDt_Min >= F.[Eff Date]
	and p.FY = f.[Aid Yr]	-- need because students can apply for future years so can't rely on effective date





-------------------------------------------- Clearing House Extended Ed -----------------------------------------------
IF OBJECT_ID('tempdb..#CHD') IS NOT NULL
    DROP TABLE #CHD
select *
into #CHD
from (
	select RowID,StuID,Career,[Degree Award Type],[Entity Group],FCD_Min
		,max(Degree) as HiPrevDegree
		,case when sum([4yr]) > 0 then 'Y' else 'N' end as PrevAtt4Yr
		,case when sum([2yr]) > 0 then 'Y' else 'N' end  as PrevAtt2Yr
		,max(coalesce(PrevEnrllDate,GradDate)) as LastAttDate
		,datediff(day,max(coalesce(PrevEnrllDate,GradDate)),FCD_Min) as DaysSinceLastAtt
	from (
		select 
			case when PrevEnrllDate <= CmpDt_Min then 1 
				when GradDate <= CmpDt_Min then 1
				end as ValidPriorEnrl
			,case when GradDate <= CmpDt_Min then DegreeW else null end as Degree
			,*
		from (	
			select p.*
				,cast( left(cast(c.[Enrollment Begin]as int),4)
					+ substring(cast(cast([Enrollment Begin]as int) as char(8)),5,2)
					+ right(cast(c.[Enrollment Begin]as int),2) 
					as date) as PrevEnrllDate
				,cast( left(cast(c.[Graduation Date]as int),4)
					+ substring(cast(cast([Graduation Date]as int) as char(8)),5,2)
					+ right(cast(c.[Graduation Date]as int),2) 
					as date) as GradDate
				,case when [Degree Title] like '%cert%' then null else (
					case when [Degree Title] like '%Assoc%' or [Degree Title] like 'AA%' or [Degree Title] like '%A.A.%' or [Degree Title] like '%A.S%' or [Degree Title] like 'AS %' or [Degree Title] like 'AS-%' or [Degree Title] like '%ASS%' or [Degree Title] like '%ssociate%' or [Degree Title] in ('AS')
							then '1 AA' 
						when [Degree Title] like '%bach%' or [Degree Title] like 'ba%' or [Degree Title] like 'bs%' or [Degree Title] like 'b %' or [Degree Title] like 'b.%' or [Degree Title] like '%achelor%' or [Degree Title] in ('bba','bpa')
							then '2 BA' 
						when [Degree Title] like '%mast%' or [Degree Title] like 'ma%' or [Degree Title] like 'ms%' or [Degree Title] like 'm %' or [Degree Title] like 'm.%' or [Degree Title] like 'mb%' or [Degree Title] like 'med %' or [Degree Title] like 'mf%' or [Degree Title] like 'mh%' or [Degree Title] like 'mp%' or [Degree Title] like '%aster%' or [Degree Title] in ('MED','MIS')
							then '3 MA' 
						when [Degree Title] like '%doctor%' or [Degree Title] like 'ED.%' or [Degree Title] like 'EDD%' or [Degree Title] like 'JD%' or [Degree Title] like '%PSY.D%' or [Degree Title] like '%PHD%' or [Degree Title] in ('dba','dph','dpt','PSYD')
							then '4 Dr'
						end) 
				 end as DegreeW
				,case when c.[2-year / 4-year] = 4 then 1 end as [4yr]
				,case when c.[2-year / 4-year] = 2 then 1 end as [2yr]
				,c.*
			from #pop p
			left join IR.dbo.CHD c
				on p.StuID = c.[Requester Return Field]
			where [College Name] <> 'NATIONAL UNIVERSITY'
			) c
		) d
		where ValidPriorEnrl = 1
		group by RowID,StuID,Career,[Degree Award Type],[Entity Group],FCD_Min
	) chd








------------------------------------------ NU Ext Ed ---------------------------------------------
IF OBJECT_ID('tempdb..#NuExtEd') IS NOT NULL
    DROP TABLE #NuExtEd
select *
into #NuExtEd
from (
	select p.RowID,p.StuID,p.Career,p.[Degree Award Type],FCD_Min
		,max(e.DegreeLvl) as HiDegr
		,max(e.DegreeDate) as MaxDegrDt
		,DATEDIFF(day,max(e.DegreeDate),p.FCD_Min) as DaysSinceDeg
		,max(e.GPA) as GPA
		,case when sum(e.[4Yr]) >= 1 then 'Y' else 'N' end as PrevAtt4yr
		,case when sum(e.[2Yr]) >= 1 then 'Y' else 'N' end as PrevAtt2yr
	from #pop p
	left join (
		select coalesce(c.EMPLID,s.ID) StudentID
			,case when c.EMPLID is not null and s.ID is not null then 'Both'
				when c.EMPLID is not null then 'College Only'
				when s.ID is not null then 'Sum Only' end as Sources
			,coalesce(c.EXT_ORG_ID,s.[Org ID]) as ORG_ID
			,c.EXT_ORG_ID
			,s.[Org ID]
			,c.DEGREE
			,c.DESCR
			,case when c.DEGREE = 'GED' then '0 HS'
				when c.DESCR like '%assoc%' then '1 AA'
				when c.DESCR like '%bach%' or c.DESCR like 'b.%' then '2 BA'
				when c.DESCR like '%mast%' then '3 MA'
				when c.DESCR like '%Doct%' then '4 Dr'
				when c.DESCR like '%High%' then '0 HS'
				end as DegreeLvl
			,case when DEGREE_DT not like '%/190%' then cast(DEGREE_DT as date) end as DegreeDate
			,c.DEGREE_STATUS
			,c.[Eff Date] as EffDateColl
			,c.[Group Name]
			,s.Career	as careerColl
			,s.[Sum Type]
			,s.[Year] 
			,case when coalesce(s.[Ext GPA],s.[Conv GPA]) <= 4 then nullif(coalesce(s.[Ext GPA],s.[Conv GPA]),0) end as GPA
			,s.[Eff Date]
			,case when c.DESCR like '%assoc%' then 1 else e.[2Yr] end as [2Yr]
			,case when c.DESCR like '%bach%' or c.DESCR like 'b.%' or c.DESCR like '%mast%' or c.DESCR like '%Doct%' then 1 else e.[4Yr] end as [4Yr]
		from ir.dbo.ExtCollege c
		full outer join ir.dbo.ExtAcadSum s
			on c.EMPLID = s.ID
			and c.EXT_ORG_ID = s.[Org ID]
		left join (				-- get 2/4yr data
			select distinct e.*
				,d.[2Yr]
				,d.[4Yr]
			from (
				select distinct [Org Type],[Org ID],Descr
				from IR.dbo.ExOrg) e
			join (
				select distinct INSTNM
					,case when ICLEVEL_L = '2yr' then 1 else 0 end as [2Yr]
					,case when ICLEVEL_L = '4yr' then 1 else 0 end as [4Yr]
				from IR.ipedsDC.InstChars_Dir_Info) d
				on e.Descr = d.INSTNM
			) e
			on coalesce(c.EXT_ORG_ID,s.[Org ID]) = e.[Org ID]
	) e
	on p.StuID = e.StudentID
	and p.CmpDt_Min >= cast(e.DegreeDate as date)
	group by p.RowID,p.StuID,p.Career,p.[Degree Award Type],FCD_Min
) e








----------------------------------- Transfer Units -------------------------------------
IF OBJECT_ID('tempdb..#trans_working') IS NOT NULL
    DROP TABLE #trans_working
select *
into #trans_working
from (
	select RowID,StuID,p.Career,[Degree Award Type],Term_Min
		,isnull(sum(c.[Units Transferred]),0) as TransUnits
		,cast(sum(c.[Units Transferred]*c.[Grd Pt Unt]) as numeric (8,3)) as TransGrdPts
		,cast(case when sum(c.[Units Transferred]) > 0 then 
			sum(c.[Units Transferred]*c.[Grd Pt Unt]) / sum(c.[Units Transferred]) end as numeric (4,3)) as TransGPA
		,sum(c.[Ext Units]) as ExtUnits
		,cast(sum(c.[Ext Units]*g.[Grd Points]) as numeric (8,3)) as ExtGrdPts
		,cast(case when sum(c.[Ext Units]) > 0 then 
			sum(c.[Ext Units]*g.[Grd Points]) / sum(c.[Ext Units]) end as numeric (4,3)) as ExtGPA
	from #pop p
	left join (select ID,Career,[Artic Term],EarnCredit,[Grade In],Grade
				,case when EarnCredit = 'Y' then [Units Transferred] end as [Units Transferred]
				,case when EarnCredit = 'Y' then [Grd Pts Per Unt] end as [Grd Pt Unt]
				,[Ext Units]
			from IR.soar.vw_NU_IR_TRNS_CRSE_DTL C
			where ID in (select StuID from #pop)) C
		on p.StuID = c.ID
		and p.Career = c.Career
		and p.Term_Min >= c.[Artic Term]	-- some students get transfer units approved just after their first term but I don't see how we would catch this in production so excluding these
	left join (select distinct  Grading,[grade in],[grd points]	--get grade points for non transferred classes
					,case when [Grd Scheme] = 'NUG' then 'UGRD' 
						when [Grd Scheme] = 'NGR' then 'GRAD' end as Car
				from IRDev.soar.SR733B__GRADE_TABLE
				where grading = 'TRN' and SetID = 'NATLU' and [Grd Scheme] in ('NGR','NUG')) g
		on c.[Grade In] = g.[Grade In]
		and p.Career = g.Car
	group by RowID,StuID,p.Career,[Degree Award Type],Term_Min
) t



IF OBJECT_ID('tempdb..#trans') IS NOT NULL
    DROP TABLE #trans
select *
into #trans
from (
	select t.*, e.PrevAtt2yr, e.PrevAtt4yr
	from #trans_working t
	left join (
		select p.RowID,p.StuID,p.[Degree Award Type],p.Term_Min
			,case when sum(e.[4Yr]) >= 1 then 'Y' else 'N' end as PrevAtt4yr
			,case when sum(e.[2Yr]) >= 1 then 'Y' else 'N' end as PrevAtt2yr
		from #pop p
		left join IR.soar.vw_NU_IR_TRNS_CRSE_DTL t
			on p.StuID = t.ID
			and p.Career = t.Career
			and p.Term_Min >= t.[Artic Term]
		left join (
			select distinct e.*
				,d.[2Yr]
				,d.[4Yr]
			from (
				select distinct [Org Type],[Org ID],Descr
				from IR.dbo.ExOrg) e
			join (
				select distinct INSTNM
					,case when ICLEVEL_L = '2yr' then 1 else 0 end as [2Yr]
					,case when ICLEVEL_L = '4yr' then 1 else 0 end as [4Yr]
				from IR.ipedsDC.InstChars_Dir_Info) d
				on e.Descr = d.INSTNM
			) e
			on t.[Source ID] = e.[Org ID]
		group by p.RowID,p.StuID,p.[Degree Award Type],p.Term_Min
	) e
	on t.RowID = e.RowID
	) a



------------------------ BRING ALL EXT ED TOGETHER -------------------------------
IF OBJECT_ID('tempdb..#ExtEd') IS NOT NULL
    DROP TABLE #ExtEd
select *
into #ExtEd
from (
	select e.*
		,case when HiDegr >= NUdeg then 'Equal or Higher' -- combining since groups are small
			else 'Lower' end as PrevDegLvl
	from (
		select RowID,StuID,Career,[Degree Award Type]
			,case when [Degree Award Type] like 'A%' then '1 AA'
				when [Degree Award Type] like 'B%' then '2 BA'
				when [Degree Award Type] like 'M%' then '3 MA'
				end as NUdeg
			,max(HiDegr) as HiDegr
			,max(DaysSinceDeg) as DaysSinceLastAtt
			,cast(max(GPA) as numeric(6,3)) as PrevGPA
			,max(TransUnits) as TransUnits
			,max(TransGPA) as TransGPA
			,max(PrevAtt4Yr) as PrevAtt4Yr	--max is Y since N < Y
			,max(PrevAtt2Yr) as PrevAtt2Yr
		from (
			select RowID,StuID,Career,[Degree Award Type],HiDegr,DaysSinceDeg,GPA,TransUnits,TransGPA
				,case when Career = 'GRAD' then 'Y' else PrevAtt4yr end as PrevAtt4yr
				,PrevAtt2yr
			from (
				select RowID,StuID,Career,[Degree Award Type]
					,HiDegr,DaysSinceDeg
					,GPA
					,null as TransUnits,null as TransGPA
					,e.PrevAtt4yr
					,e.PrevAtt2yr
				from #NuExtEd e
					union 
				select RowID,StuID,Career,[Degree Award Type]
					,HiPrevDegree,DaysSinceLastAtt
					,null as GPA
					,null as TransUnits,null as TransGPA
					,c.PrevAtt4Yr,c.PrevAtt2Yr
				from #CHD c
					union
				select RowID,StuID,Career,[Degree Award Type]
					,null as HiPrevDegree,null as DaysSinceLastAtt
					,coalesce(t.ExtGPA,t.TransGPA) as GPA
					,t.TransUnits, t.TransGPA
					,t.PrevAtt4yr,t.PrevAtt2yr
				from #trans t
				) u
			) p
		group by RowID,StuID,Career,[Degree Award Type]
			,case when [Degree Award Type] like 'A%' then '1 AA'
				when [Degree Award Type] like 'B%' then '2 BA'
				when [Degree Award Type] like 'M%' then '3 MA' end
		) e
	) f
	

-------------------------------------------------------------- FINAL ANALYSIS TABLE ------------------------------------------------
IF OBJECT_ID('tempdb..#analysis') IS NOT NULL
    DROP TABLE #analysis
select *
into #analysis
from (
select p.RowID
	,p.StuID
	,o.Dropout
	,p.[Degree Award Type] as DegreeAwardType
	,d.Gender
	,d.Ethnicity
	,d.Age
	,d.ActiveDuty
	,d.MilitaryYN
	,d.AGI_PerCapita
	,d.Probation
	,d.CIP2D
	,e.DaysSinceLastAtt
	,e.PrevDegLvl
	,e.PrevAtt4Yr
	,e.PrevAtt2Yr
	,e.PrevGPA
	,e.TransUnits
	,fa.FAFSAby
	,fa.ParentHiEd
	,fa.AGI
	,fa.GovtPrgmY
	,fa.DependentsY
	,fa.ChildrenY
	,fa.MaritalStat
	,fa.EFC
	,fc.ClassLength_avg
	,fc.Classes
	,fc.Units
	,fc.DFUWI
	,fc.OnlineOnly
	,fc.AdjunctOnly
	,fc.RemedialCrs
	,fc.LearningGrp
	,fc.TeachingGrp
	,fc.RelativePerf
	,fc.SIC_avg
	,fc.ClassCPE
	,sc.DaysToFC
	,sc.DaysToFYClose
	,sc.FCD_FYQ
	,isnull(si.N_BCW,'N') as N_BCW
	,isnull(si.N_BKA,'N') as N_BKA
	,isnull(si.N_BLK,'N') as N_BLK
	,isnull(si.N_BPP,'N') as N_BPP
	,isnull(si.N_CRD,'N') as N_CRD
	,isnull(si.N_ECD,'N') as N_ECD
	,isnull(si.N_FPW,'N') as N_FPW
	,isnull(si.N_Hold,'N') as N_Hold
	,isnull(si.N_IntlStu,'N') as N_IntlStu
	,isnull(si.N_REC,'N') as N_REC
	,isnull(si.N_WriteOff,'N') as N_WriteOff
	,isnull(si.P_B2B,'N') as P_B2B
	,isnull(si.P_BCB,'N') as P_BCB
	,isnull(si.P_BCR,'N') as P_BCR
	,isnull(si.P_BOM,'N') as P_BOM
	,isnull(si.P_BRR,'N') as P_BRR
	,isnull(si.P_BSP,'N') as P_BSP
	,isnull(si.P_BSS,'N') as P_BSS
	,isnull(si.P_BV3,'N') as P_BV3
	,isnull(si.P_BVA,'N') as P_BVA
	,isnull(si.P_BVO,'N') as P_BVO
	,isnull(si.P_ECP,'N') as P_ECP
	,isnull(si.P_ECS,'N') as P_ECS
	,isnull(si.P_EPS,'N') as P_EPS
	,isnull(si.P_FAC,'N') as P_FAC
	,isnull(si.P_FAO,'N') as P_FAO
	,isnull(si.P_FAS,'N') as P_FAS
	,isnull(si.P_FBA,'N') as P_FBA
	,isnull(si.P_FRD,'N') as P_FRD
	,isnull(si.P_HIC,'N') as P_HIC
	,isnull(si.P_MiscSchlr,'N') as P_MiscSchlr
	,isnull(si.P_NOS,'N') as P_NOS
	,isnull(si.P_OffsiteDisc,'N') as P_OffsiteDisc
	,isnull(si.P_REV,'N') as P_REV
	,isnull(si.P_RFA,'N') as P_RFA
	,isnull(si.P_ROR,'N') as P_ROR
	,isnull(si.P_SOC,'N') as P_SOC
	,isnull(si.P_VaTuit,'N') as P_VaTuit
from #pop p
left join #outcome o	on p.RowID = o.RowID 
left join #demo d		on p.RowID = d.RowID 
left join #ExtEd e		on p.RowID = e.RowID 
left join #FAFSA fa		on p.RowID = fa.RowID 
left join #FirstClass fc on p.RowID = fc.RowID 
left join #Schedule sc	on p.RowID = sc.RowID 
left join #ServInd si	on p.RowID = si.RowID 
where d.BioDataExists = 1 --excluding these because of JFK data merge
) a




------------------------------ TEMPORARILITY STORE THE DATA SET --------------------------------
drop table ir.dbo.dropout
select * into ir.dbo.dropout from #analysis 

select *
from IR.dbo.dropout
order by RowID





