USE [welcomgroup]
GO

/****** Object:  StoredProcedure [hotels].[SP_GetAllPlanRatesByHotel_mod]    Script Date: 05/27/2016 22:30:18 ******/
IF  EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[hotels].[SP_GetAllPlanRatesByHotel_mod]') AND type in (N'P', N'PC'))
DROP PROCEDURE [hotels].[SP_GetAllPlanRatesByHotel_mod]
GO

USE [welcomgroup]
GO

/****** Object:  StoredProcedure [hotels].[SP_GetAllPlanRatesByHotel_mod]    Script Date: 05/27/2016 22:30:18 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

-- =============================================
-- Description:	<To get all plan rates by room>
-- =============================================
CREATE PROCEDURE [hotels].[SP_GetAllPlanRatesByHotel_mod] 
@hotelId numeric(18,0),
@checkInDate datetime,
@checkOutDate datetime,
@noOfDays int,
@advDays int
AS
BEGIN

    declare @tempCheckInDate datetime
	declare @tempCheckOutDate datetime
	declare @tempIndCheckInDate datetime
	declare @tempIndCheckOutDate datetime
	declare @dontChgCheckInDate datetime
	Declare @planCheckInDate datetime
	Declare @planCheckOutDate datetime
	set @tempCheckInDate = convert(DATE,@checkInDate)
	set @tempIndCheckInDate = convert(DATE,@checkInDate)
	set @dontChgCheckInDate = convert(DATE,@checkInDate)
	set @planCheckInDate = convert(DATE,@checkInDate)
	set @planCheckOutDate = convert(DATE,@checkOutDate)
	
	if(@noOfDays > 7)
	begin
	   set @tempCheckOutDate = DATEADD(DAY,7,@tempCheckInDate)
	end
	else
	begin
	   set @tempCheckOutDate = @checkOutDate
	end

 CREATE TABLE #temp_plan_rates
	(
	 curr_date varchar(20),amt_rspax1 numeric(18,2),amt_rspax2 numeric(18,2),amt_rspaxex numeric(18,2),
	 text_content1 varchar(8000),Desc_plan varchar(50),id_plan numeric(18,0),code_classification char(1),
	 no_priority int,plan_terms varchar(5000),inclusion_desc varchar(5000),Flg_FreeNight char(1),flg_redemption varchar(1),
	 redemptionType varchar(1),points numeric(18,0),
	 DepositType char(1),DepositValue int,CancelType char(1),CancelValue int,CancelDays int,CancelTime varchar(5),
	 flg_breakfast char(1),flg_wifi char(1),flg_one_major_meals char(1),flg_spa_treatment char(1),flg_two_major_meals char(1),
	 Flg_avlday1 char(1),Flg_avlday2 char(1),Flg_avlday3 char(1),Flg_avlday4 char(1),Flg_avlday5 char(1),
	 Flg_avlday6 char(1),Flg_avlday7 char(1),Flg_arrday1 char(1),Flg_arrday2 char(1),Flg_arrday3 char(1),
	 Flg_arrday4 char(1),Flg_arrday5 char(1),Flg_arrday6 char(1),Flg_arrday7 char(1)
	)
	
	CREATE TABLE #temp_independent_plan_rates
	(
	 curr_date varchar(20),amt_rspax1 numeric(18,2),amt_rspax2 numeric(18,2),amt_rspaxex numeric(18,2),
	 text_content1 varchar(8000),Desc_plan varchar(50),id_plan numeric(18,0),code_classification char(1),
	 no_priority int,plan_terms varchar(5000),inclusion_desc varchar(5000),Flg_FreeNight char(1),flg_redemption varchar(1),
	 redemptionType varchar(1),points numeric(18,0),
	 DepositType char(1),DepositValue int,CancelType char(1),CancelValue int,CancelDays int,CancelTime varchar(5),
	 flg_breakfast char(1),flg_wifi char(1),flg_one_major_meals char(1),flg_spa_treatment char(1),flg_two_major_meals char(1),
	 Flg_avlday1 char(1),Flg_avlday2 char(1),Flg_avlday3 char(1),Flg_avlday4 char(1),Flg_avlday5 char(1),
	 Flg_avlday6 char(1),Flg_avlday7 char(1),Flg_arrday1 char(1),Flg_arrday2 char(1),Flg_arrday3 char(1),
	 Flg_arrday4 char(1),Flg_arrday5 char(1),Flg_arrday6 char(1),Flg_arrday7 char(1)
	)

 while(@checkInDate < @checkOutDate)
 begin
    INSERT INTO #temp_plan_rates
	(
	 curr_date,amt_rspax1,amt_rspax2,amt_rspaxex,text_content1,Desc_plan,id_plan,code_classification,
	 no_priority,plan_terms,inclusion_desc,Flg_FreeNight,flg_redemption,
	 redemptionType,points,DepositType,DepositValue,CancelType,CancelValue,CancelDays,CancelTime,
	 flg_breakfast,flg_wifi,flg_one_major_meals,flg_spa_treatment,flg_two_major_meals,
	 Flg_avlday1,Flg_avlday2,Flg_avlday3,Flg_avlday4,Flg_avlday5,
	 Flg_avlday6,Flg_avlday7,Flg_arrday1,Flg_arrday2,Flg_arrday3,
	 Flg_arrday4,Flg_arrday5,Flg_arrday6,Flg_arrday7
	)
		select CONVERT(DATE,@checkInDate) 
		--,bpr.amt_rspax1,bpr.amt_rspax2
		 ,case when bpr.amt_rspax1=0 and bp.redemptionType = 'F' and bp.flg_redemption = 'Y' then bpr.points else bpr.amt_rspax1 end as amt_rspax1
		 ,case when bpr.amt_rspax2=0 and bp.redemptionType = 'F' and bp.flg_redemption = 'Y' then bpr.points else bpr.amt_rspax2 end as amt_rspax2
		,bpr.amt_rspaxex, bp.text_content1,bp.Desc_plan, bp.id_plan, 
		bp.code_classification,bp.no_priority, bp.plan_terms, bp.inclusion_desc, bp.Flg_FreeNight, bp.flg_redemption, 
		bp.redemptionType, bpr.points,bp.DepositType, bp.DepositValue, bp.CancelType, bp.CancelValue, bp.CancelDays, bp.CancelTime,
		flg_breakfast,flg_wifi,flg_one_major_meals,flg_spa_treatment,flg_two_major_meals,
		Flg_avlday1,Flg_avlday2,Flg_avlday3,Flg_avlday4,Flg_avlday5,
		Flg_avlday6,Flg_avlday7,Flg_arrday1,Flg_arrday2,Flg_arrday3,
		Flg_arrday4,Flg_arrday5,Flg_arrday6,Flg_arrday7 
		from dbo.b_room br inner join dbo.b_room_avail bravail 
		on br.Id_room=bravail.id_room inner join dbo.b_plan_rate bpr on br.Id_room=bpr.id_room 
		inner join dbo.b_plan bp on bpr.id_plan=bp.id_plan  
		where br.id_hotel= @hotelId and bravail.Date_avail = @checkInDate and bravail.Flg_status='O' and 
		bp.flg_del = 'N' and bp.soft_delete='N' and @checkInDate between date_startocc and date_endocc
		and bp.id_plan in (
	    Select bp2.id_plan from b_plan_rate bpr2 inner join b_plan bp2 
		inner join b_planstatus bps on bp2.id_plan = bps.id_plan
		on bpr2.id_plan = bp2.id_plan where bp2.id_hotel = @hotelId and bp2.DiscountValue is not null
		and @noOfDays between bp2.no_minlos and bp2.no_maxlos and bp2.flg_del = 'N' and bp2.soft_delete='N'
		and (bp2.promo_code is null or bp2.promo_code = '') and bps.Flg_openstatus = 'O' 
		and bps.Date_planstatus = @checkInDate and bp2.no_advdays <= @advDays and bpr2.date_startocc <= @planCheckInDate
		and bpr2.date_endocc >= @planCheckOutDate 
		union Select bp2.id_plan from b_plan_rate bpr2 inner join b_plan bp2 
		on bpr2.id_plan = bp2.id_plan inner join b_planstatus bps on bp2.id_plan = bps.id_plan 
		where bp2.id_hotel = @hotelId and UPPER(bp2.rate_code) = 'NONE'
		and @noOfDays between bp2.no_minlos and bp2.no_maxlos and bp2.flg_del = 'N' and bp2.soft_delete='N'
		and (bp2.promo_code is null or bp2.promo_code = '') and bps.Flg_openstatus = 'O' 
		and bps.Date_planstatus = @checkInDate and bp2.no_advdays <= @advDays and bpr2.date_startocc <= @planCheckInDate
		and bpr2.date_endocc >= @planCheckOutDate)
		order by bpr.amt_rspax1,bpr.amt_rspax2,bpr.amt_rspaxex
		
		set @checkInDate = DATEADD(day,1,@checkInDate)  
  end
      
  ----For Independent--
  while(@tempIndCheckInDate < @checkOutDate)
 begin
    INSERT INTO #temp_independent_plan_rates
	(
	 curr_date,amt_rspax1,amt_rspax2,amt_rspaxex,text_content1,Desc_plan,id_plan,code_classification,
	 no_priority,plan_terms,inclusion_desc,Flg_FreeNight,flg_redemption,
	 redemptionType,points,DepositType,DepositValue,CancelType,CancelValue,CancelDays,CancelTime,
	 flg_breakfast,flg_wifi,flg_one_major_meals,flg_spa_treatment,flg_two_major_meals,
	 Flg_avlday1,Flg_avlday2,Flg_avlday3,Flg_avlday4,Flg_avlday5,
	 Flg_avlday6,Flg_avlday7,Flg_arrday1,Flg_arrday2,Flg_arrday3,
	 Flg_arrday4,Flg_arrday5,Flg_arrday6,Flg_arrday7
	)
		select CONVERT(DATE,@tempIndCheckInDate)
		 --, bpr.amt_rspax1,bpr.amt_rspax2
		,case when bpr.amt_rspax1=0 and bp.redemptionType = 'F' and bp.flg_redemption = 'Y' then bpr.points else bpr.amt_rspax1 end as amt_rspax1
		,case when bpr.amt_rspax2=0 and bp.redemptionType = 'F' and bp.flg_redemption = 'Y' then bpr.points else bpr.amt_rspax2 end as amt_rspax2
		,bpr.amt_rspaxex, bp.text_content1,bp.Desc_plan, bp.id_plan, 
		bp.code_classification,bp.no_priority, bp.plan_terms, bp.inclusion_desc, bp.Flg_FreeNight, bp.flg_redemption, 
		bp.redemptionType, bpr.points,bp.DepositType, bp.DepositValue, bp.CancelType, bp.CancelValue, bp.CancelDays, bp.CancelTime,
		flg_breakfast,flg_wifi,flg_one_major_meals,flg_spa_treatment,flg_two_major_meals,
		Flg_avlday1,Flg_avlday2,Flg_avlday3,Flg_avlday4,Flg_avlday5,
		Flg_avlday6,Flg_avlday7,Flg_arrday1,Flg_arrday2,Flg_arrday3,
		Flg_arrday4,Flg_arrday5,Flg_arrday6,Flg_arrday7 
		from dbo.b_room br inner join dbo.b_room_avail bravail 
		on br.Id_room=bravail.id_room inner join dbo.b_plan_rate bpr on br.Id_room=bpr.id_room 
		inner join dbo.b_plan bp on bpr.id_plan=bp.id_plan  
		where br.id_hotel= @hotelId and bravail.Date_avail = @tempIndCheckInDate and bravail.Flg_status='O' and 
		bp.flg_del = 'N' and bp.soft_delete='N'  
		and bp.id_plan in (Select bp2.id_plan from b_plan_rate bpr2 inner join b_plan bp2 
		on bpr2.id_plan = bp2.id_plan inner join b_ratecategory brc on bp2.rate_code = brc.rate_code
		inner join b_ratecategory_date brd on brc.id_ratecategory = brd.id_ratecategory_date
		inner join b_planstatus bps on bp2.id_plan = bps.id_plan   
		where bp2.id_hotel = @hotelId and brd.Date_planstatus = convert(DATE,@tempIndCheckInDate) and brd.Flg_openstatus = 'O'
		and @noOfDays between bp2.no_minlos and bp2.no_maxlos and bp2.flg_del = 'N' and bp2.soft_delete='N'
		and (bp2.promo_code is null or bp2.promo_code = '') and bps.Flg_openstatus = 'O' 
		and bps.Date_planstatus = @tempIndCheckInDate and bp2.no_advdays <= @advDays)
		order by bpr.amt_rspax1,bpr.amt_rspax2,bpr.amt_rspaxex
		
		set @tempIndCheckInDate = DATEADD(day,1,@tempIndCheckInDate)  
  end
   
   set @checkInDate = @tempCheckInDate
    declare @YFlag varchar(1) = 'Y' 
    declare @columnStr varchar(300)=''
    declare @sql nvarchar(1000)
	declare @daystr varchar(20)=''
	declare @arrDayStr varchar(20)=''
	declare @arrDayCol varchar(50)=''
	set @arrDayStr = DATENAME(DW,@tempCheckInDate)
	
	
		if(@arrDayStr = 'Monday')
		begin 
		  set @arrDayCol = 'Flg_arrday1='''+@YFlag+''''
		end
		else if(@arrDayStr = 'Tuesday')
		begin 
		  set @arrDayCol = 'Flg_arrday2='''+@YFlag+''''
		end
		else if(@arrDayStr = 'Wednesday')
		begin 
		  set @arrDayCol = 'Flg_arrday3='''+@YFlag+''''
		end
		else if(@arrDayStr = 'Thursday')
		begin 
		  set @arrDayCol = 'Flg_arrday4='''+@YFlag+''''
		end
		else if(@arrDayStr = 'Friday')
		begin  
		  set @arrDayCol = 'Flg_arrday5='''+@YFlag+''''
		end
		else if(@arrDayStr = 'Saturday')
		begin 
		  set @arrDayCol = 'Flg_arrday6='''+@YFlag+''''
		end
		else if(@arrDayStr = 'Sunday')
		begin 
		  set @arrDayCol = 'Flg_arrday7='''+@YFlag+''''
		end
    
    while(@tempCheckInDate < CONVERT(DATE,@tempCheckOutDate))
    begin
		set @daystr = DATENAME(DW,@tempCheckInDate)
		
		if(@columnStr != '')
		begin
			set @columnStr = @columnStr + ' and '
		end
		if(@daystr = 'Monday')
		begin 
		  set @columnStr = @columnStr + 'Flg_avlday1='''+@YFlag+''''
		end
		else if(@daystr = 'Tuesday')
		begin 
		  set @columnStr = @columnStr + 'Flg_avlday2='''+@YFlag+''''
		end
		else if(@daystr = 'Wednesday')
		begin 
		  set @columnStr = @columnStr + 'Flg_avlday3='''+@YFlag+''''
		end
		else if(@daystr = 'Thursday')
		begin 
		  set @columnStr = @columnStr + 'Flg_avlday4='''+@YFlag+''''
		end
		else if(@daystr = 'Friday')
		begin  
		  set @columnStr = @columnStr + 'Flg_avlday5='''+@YFlag+''''
		end
		else if(@daystr = 'Saturday')
		begin 
		  set @columnStr = @columnStr + 'Flg_avlday6='''+@YFlag+''''
		end
		else if(@daystr = 'Sunday')
		begin 
		  set @columnStr = @columnStr + 'Flg_avlday7='''+@YFlag+''''
		end
		
		set @tempCheckInDate = DATEADD(day,1,@tempCheckInDate)
		 
    end
    
		set @sql = 'select min(amt_rspax1),min(amt_rspax2),min(points),curr_date,text_content1,Desc_plan,id_plan, 
		code_classification,no_priority,plan_terms,inclusion_desc,Flg_FreeNight,flg_redemption, 
		redemptionType,DepositType,DepositValue,CancelType,CancelValue,CancelDays,CancelTime,
		flg_breakfast,flg_wifi,flg_one_major_meals,flg_spa_treatment,flg_two_major_meals from #temp_plan_rates where ' +@columnStr+' and '
		+@arrDayCol+' group by curr_date,text_content1,Desc_plan,id_plan, 
		code_classification,no_priority,plan_terms,inclusion_desc,Flg_FreeNight,flg_redemption, 
		redemptionType,DepositType,DepositValue,CancelType,CancelValue,CancelDays,CancelTime,
		flg_breakfast,flg_wifi,flg_one_major_meals,flg_spa_treatment,flg_two_major_meals order by curr_date asc' 
		
	    create table #temp_min_rate_by_dates
	    (amt_rspax1 numeric(18,2),amt_rspax2 numeric(18,2),points numeric(18,0),curr_date varchar(20),text_content1 varchar(8000),Desc_plan varchar(50),id_plan numeric(18,0),
	     code_classification char(1),no_priority int,plan_terms varchar(5000),inclusion_desc varchar(5000),Flg_FreeNight char(1),
	     flg_redemption varchar(1),redemptionType varchar(1),
		 DepositType char(1),DepositValue int,CancelType char(1),CancelValue int,CancelDays int,CancelTime varchar(5),
		 flg_breakfast char(1),flg_wifi char(1),flg_one_major_meals char(1),flg_spa_treatment char(1),flg_two_major_meals char(1)
	    )
		
		INSERT INTO #temp_min_rate_by_dates
		(amt_rspax1,amt_rspax2,points,curr_date,text_content1,Desc_plan,id_plan, 
		code_classification,no_priority,plan_terms,inclusion_desc,Flg_FreeNight,flg_redemption, 
		redemptionType,DepositType,DepositValue,CancelType,CancelValue,CancelDays,CancelTime,
		flg_breakfast,flg_wifi,flg_one_major_meals,flg_spa_treatment,flg_two_major_meals) 
	    
	    execute sp_executesql @sql
	       
		---For Independent--
		declare @indSql nvarchar(4000)
		set @indSql = 'select min(amt_rspax1),min(amt_rspax2),min(points),curr_date,text_content1,Desc_plan,id_plan, 
		code_classification,no_priority,plan_terms,inclusion_desc,Flg_FreeNight,flg_redemption, 
		redemptionType,DepositType,DepositValue,CancelType,CancelValue,CancelDays,CancelTime,
		flg_breakfast,flg_wifi,flg_one_major_meals,flg_spa_treatment,flg_two_major_meals from #temp_independent_plan_rates where ' +@columnStr+' and '
		+@arrDayCol+' group by curr_date,text_content1,Desc_plan,id_plan, 
		code_classification,no_priority,plan_terms,inclusion_desc,Flg_FreeNight,flg_redemption, 
		redemptionType,DepositType,DepositValue,CancelType,CancelValue,CancelDays,CancelTime,
		flg_breakfast,flg_wifi,flg_one_major_meals,flg_spa_treatment,flg_two_major_meals order by curr_date asc' 
		
	    create table #temp_ind_min_rate_by_dates
	    (amt_rspax1 numeric(18,2),amt_rspax2 numeric(18,2),points numeric(18,0),curr_date varchar(20),text_content1 varchar(8000),Desc_plan varchar(50),id_plan numeric(18,0),
	     code_classification char(1),no_priority int,plan_terms varchar(5000),inclusion_desc varchar(5000),Flg_FreeNight char(1),
	     flg_redemption varchar(1),redemptionType varchar(1),
		 DepositType char(1),DepositValue int,CancelType char(1),CancelValue int,CancelDays int,CancelTime varchar(5),
		 flg_breakfast char(1),flg_wifi char(1),flg_one_major_meals char(1),flg_spa_treatment char(1),flg_two_major_meals char(1)
	    )
		
		INSERT INTO #temp_ind_min_rate_by_dates
		(amt_rspax1,amt_rspax2,points,curr_date,text_content1,Desc_plan,id_plan, 
		code_classification,no_priority,plan_terms,inclusion_desc,Flg_FreeNight,flg_redemption, 
		redemptionType,DepositType,DepositValue,CancelType,CancelValue,CancelDays,CancelTime,
		flg_breakfast,flg_wifi,flg_one_major_meals,flg_spa_treatment,flg_two_major_meals) 
	    
	    execute sp_executesql @indSql
	    
	    declare @avgPax1Amt numeric(18,0)
	    declare @avgPax2Amt numeric(18,0)
	    select @avgPax1Amt = avg(amt_rspax1),@avgPax2Amt = avg(amt_rspax2) from #temp_ind_min_rate_by_dates
	    
	    create table #temp_ind_final_min_rate_by_dates
	    (amt_rspax1 numeric(18,2),amt_rspax2 numeric(18,2),points numeric(18,0),curr_date varchar(20),text_content1 varchar(8000),Desc_plan varchar(50),id_plan numeric(18,0),
	     code_classification char(1),no_priority int,plan_terms varchar(5000),inclusion_desc varchar(5000),Flg_FreeNight char(1),
	     flg_redemption varchar(1),redemptionType varchar(1),
		 DepositType char(1),DepositValue int,CancelType char(1),CancelValue int,CancelDays int,CancelTime varchar(5),
		 flg_breakfast char(1),flg_wifi char(1),flg_one_major_meals char(1),flg_spa_treatment char(1),flg_two_major_meals char(1)
	    )
	    
	    INSERT INTO #temp_ind_final_min_rate_by_dates
		(amt_rspax1,amt_rspax2,points,curr_date,text_content1,Desc_plan,id_plan, 
		code_classification,no_priority,plan_terms,inclusion_desc,Flg_FreeNight,flg_redemption, 
		redemptionType,DepositType,DepositValue,CancelType,CancelValue,CancelDays,CancelTime,
		flg_breakfast,flg_wifi,flg_one_major_meals,flg_spa_treatment,flg_two_major_meals)
	    
	    Select * from #temp_ind_min_rate_by_dates where curr_date = @dontChgCheckInDate
	    update #temp_ind_final_min_rate_by_dates set amt_rspax1=@avgPax1Amt,amt_rspax2=@avgPax2Amt where curr_date = @dontChgCheckInDate
	   
	    create table #temp_final_min_rate
	    (amt_rspax1 numeric(18,2),amt_rspax2 numeric(18,2),text_content1 varchar(8000),Desc_plan varchar(50),id_plan numeric(18,0),
	     code_classification char(1),no_priority int,plan_terms varchar(5000),inclusion_desc varchar(5000),Flg_FreeNight char(1),
	     flg_redemption varchar(1),redemptionType varchar(1),
		 DepositType char(1),DepositValue int,CancelType char(1),CancelValue int,CancelDays int,CancelTime varchar(5),
		 flg_breakfast char(1),flg_wifi char(1),flg_one_major_meals char(1),flg_spa_treatment char(1),flg_two_major_meals char(1)
	    )
	    INSERT INTO #temp_final_min_rate
		(amt_rspax1,amt_rspax2,text_content1,Desc_plan,id_plan, 
		code_classification,no_priority,plan_terms,inclusion_desc,Flg_FreeNight,flg_redemption, 
		redemptionType,DepositType,DepositValue,CancelType,CancelValue,CancelDays,CancelTime,
		flg_breakfast,flg_wifi,flg_one_major_meals,flg_spa_treatment,flg_two_major_meals)
		
		select 
	    --avg(amt_rspax1) as amt_rspax1,avg(amt_rspax2) as amt_rspax2,min(points) as points
	    case when redemptionType = 'F' then min(points) else avg(amt_rspax1) end as amt_rspax1
	    ,case when redemptionType = 'F' then min(points) else avg(amt_rspax2) end as amt_rspax2
	    ,text_content1,Desc_plan,id_plan, 
		code_classification,no_priority,plan_terms,inclusion_desc,Flg_FreeNight,flg_redemption, 
		redemptionType,DepositType,DepositValue,CancelType,CancelValue,CancelDays,CancelTime,
		flg_breakfast,flg_wifi,flg_one_major_meals,flg_spa_treatment,flg_two_major_meals
		from #temp_min_rate_by_dates 
	    group by text_content1,Desc_plan,id_plan, 
		code_classification,no_priority,plan_terms,inclusion_desc,Flg_FreeNight,flg_redemption, 
		redemptionType,DepositType,DepositValue,CancelType,CancelValue,CancelDays,CancelTime,
		flg_breakfast,flg_wifi,flg_one_major_meals,flg_spa_treatment,flg_two_major_meals
		order by min(amt_rspax1) ASC
		
		INSERT INTO #temp_final_min_rate
		(amt_rspax1,amt_rspax2,text_content1,Desc_plan,id_plan, 
		code_classification,no_priority,plan_terms,inclusion_desc,Flg_FreeNight,flg_redemption, 
		redemptionType,DepositType,DepositValue,CancelType,CancelValue,CancelDays,CancelTime,
		flg_breakfast,flg_wifi,flg_one_major_meals,flg_spa_treatment,flg_two_major_meals)
		
		Select amt_rspax1,amt_rspax2,text_content1,Desc_plan,id_plan, 
		code_classification,no_priority,plan_terms,inclusion_desc,Flg_FreeNight,flg_redemption, 
		redemptionType,DepositType,DepositValue,CancelType,CancelValue,CancelDays,CancelTime,
		flg_breakfast,flg_wifi,flg_one_major_meals,flg_spa_treatment,flg_two_major_meals from 
		#temp_ind_final_min_rate_by_dates
		
		Select amt_rspax1,amt_rspax2,text_content1,Desc_plan,id_plan, 
		code_classification,no_priority,plan_terms,inclusion_desc,Flg_FreeNight,flg_redemption, 
		redemptionType,DepositType,DepositValue,CancelType,CancelValue,CancelDays,CancelTime,
		flg_breakfast,flg_wifi,flg_one_major_meals,flg_spa_treatment,flg_two_major_meals from 
		#temp_final_min_rate order by amt_rspax1 ASC
END

GO


