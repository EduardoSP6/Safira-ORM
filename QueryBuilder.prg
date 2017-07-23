<<<<<<< HEAD
#include "hbclass.ch"
#include "common.ch"
#include "objects.ch"
#include "database.ch"
#include "error.ch"
#include "adordd.ch"
#include "sqlrdd.ch"
#include "mysql.ch"
#include "sqlodbc.ch"

********************************************************************************
* Class : QueryBuilder from Safira ORM library - perform SQL instructions to database
* Date  : 18/04/2017
* Author: Eduardo P. Sales
********************************************************************************

Class QueryBuilder

	Data table AS String
	Data primaryKey 
	Data autoIncrement AS LOGICAL INIT .F.
	Data dbInstance
	Data cQuery AS String
	Data lforUpdate AS LOGICAL INIT .F.
	Data aStruct INIT hash()
	Data primaryKey AS String  init "id" // primary key field name
	Data rddName AS String init "SQLRDD" // rdd name
	
	Method New() CONSTRUCTOR
	
	Method setStruct()
	Method getStruct()
	
	Method setPrimaryKey()
	Method getPrimaryKey()
	
	Method getTable() 
	Method setTable()
	
	Method create() 
	Method select(strColumns, lforUpdate, cConditional, cOrder)
	Method where()
	Method orderBy()
	Method insert(aColmun, aData)
	Method update(aData)
	Method delete(id, cConditional)
	Method leftJoin()
	Method fullJoin()
	
	Method getNewId()
	Method arrayToList(aArray, cDelimiter, lValue)
	Method formatDate2SQL(dData, lDateTime)
	Method cValueToSQL(uData)
		
	Error Handler ShowError()
		
EndClass

********************************************************************************
Method New(table, dbInstance, cRddName) Class QueryBuilder

	::table         := table
	::dbInstance    := dbInstance
	::rddName       := if(cRddName != nil, Upper(cRddName), ::rddName)
	
	::setStruct()
	
Return Self

********************************************************************************
Method setStruct() Class QueryBuilder
	// set table-model scope
	local cQuery, oRecordSet, oError, aColumn
	local aArray:= {}
	local cMsgErro:= ""
	
	if ::dbInstance == nil
		return nil
	endif

	* This SQL instruction retreive an array containing the specified table structure. Where:
	* first position = Field's name
	* second         = Data type
	* third          = if field is nullable: (YES/NO)
	* forth          = The Key Type:
	* If Key is PRI, the column is a PRIMARY KEY or is one of the columns in a multiple-column PRIMARY KEY.
	* If Key is UNI, the column is the first column of a unique-valued index that cannot contain NULL values.
	* If Key is MUL, multiple occurrences of a given value are permitted within the column. The column is the first 
	* fifth          = Default value, if exists. Ex: 0, NULL, .f., etc.
	* sixth          = Extra information about the field. Ex: Auto_increment

	cQuery:= "SHOW COLUMNS FROM "+ ::getTable()
		
	   
   Try
	
		if ::rddName == "SQLRDD"
		
			::dbInstance:Exec(cQuery, .t., .t., @aArray)	
		   
		elseif ::rddName $ "ADO"
	
			oRecordSet:= ::dbInstance:Execute(cQuery)	
         
			aArray    := oRecordSet:GetRows()
         
	   endif
      
	Catch oError

	   cMsgErro := "Error: "  + Transform(oError:GenCode, nil) + ";" +HB_OsNewLine()+ ;
         	 		"SubC: "   + Transform(oError:SubCode, nil) + ";" +HB_OsNewLine()+ ;
         	 		"OSCode: "  + Transform(oError:OsCode,  nil) + ";" +HB_OsNewLine()+ ;
         	 		"SubSystem: " + Transform(oError:SubSystem, nil) + ";" +HB_OsNewLine()+ ;
         	 		"Mensangem: " + oError:Description
   
   End
	   
	if ! empty(cMsgErro) .OR. len(aArray) == 0
		return nil
	endif
	

	// transform the array of table structure to the class attribute
	
	for i:= 1 to len(aArray)
	   
		initValue:= nil
	   aColumn  := {=>}
	   
		// define default value to the column
		if aArray[i,5] != nil
		
			initValue := aArray[i,5]
		
		else
			
			if Left(aArray[i,2],4) == 'char'

				initValue:= Space(1)
			
			elseif aArray[i,2] $ 'varchar'
			
				initValue:= Space( Substr( aArray[i,2], 9, (At(')', aArray[i,2], 9) -1) ) )
			
			elseif aArray[i,2] $ 'text'

				initValue:= Space(255)
			
			elseif aArray[i,2] $ 'int' .OR. aArray[i,2] $ 'float'
				
				initValue:= 0
			
			elseif aArray[i,2] $ 'double' .OR. aArray[i,2] $ 'decimal' .OR. aArray[i,2] $ 'real'
			
				initValue:= 0.00
			
			elseif aArray[i,2] $ 'date'
				
				initValue:= ctod("")
					
			endif

		endif
		
		// set primary key
		if aArray[i,4] == "PRI"

			::setPrimaryKey( Alltrim(aArray[i,1]) )
			
			// check if primary key is auto increment
         if aArray[i,6] == "auto_increment" 
         	::autoIncrement := .T.
         endif

      endif

	   
		aColumn:= {=>} // fields array
	   
	   for c:= 1 to 4
	   
	   	if c == 1
	   		
	   		HSet(aColumn, 'field', Alltrim(aArray[i,1])) // field's name 
	   		
	   	elseif c == 2
	   	   
				HSet(aColumn, 'nullable', aArray[i,3]) // if field is nullable
	   	   
			elseif c == 3
	   	
	   		HSet(aColumn, 'ktype', aArray[i,4]) // field key type
	   		
			elseif c == 4
	      
	      	HSet(aColumn, 'default', cValToChar(initValue)) // default value
	      	
			endif
	   next

		HSet( ::aStruct, Alltrim(Str(i)), aColumn )

	next
	
Return nil

********************************************************************************
Method getStruct() Class QueryBuilder
	// retrieve the table-model scope
Return ::aStruct

********************************************************************************
Method setPrimaryKey(oKey) Class QueryBuilder
	::primaryKey := oKey
Return nil

********************************************************************************
Method getPrimaryKey() Class QueryBuilder

Return ::primaryKey

********************************************************************************
Method getTable() Class QueryBuilder
	// return table name 
Return ::table

********************************************************************************
Method setTable(cTable) Class QueryBuilder
	// set table name 
	::table := cTable	
Return nil 

********************************************************************************
Method ShowError() Class QueryBuilder
	// error handler
	local cMsg   := __GetMessage() 
	local nError := SubStr(cMsg, 1, 1)
 
 	_ClsSetError( _GenError( nError, ::ClassName(), cMsg ) ) 
 			
Return nil
 
********************************************************************************
Method create() Class QueryBuilder
	// create new blank Model, return a hash with field names as keys
	local aRecord:= {=>}

	for i:= 1 to len(::aStruct)
		HSet(aRecord, ::aStruct[Alltrim(Str(i))]['field'], ::aStruct[Alltrim(Str(i))]['default'])
	next

return aRecord

********************************************************************************
Method select(strColumns, lforUpdate, cConditional, cOrder) Class QueryBuilder
	// perform SQL Select instruction	

	local cColumns, oRecordSet 
	local aResult:= {}
	
	cColumns    := if(strColumns != nil, strColumns, "*")
	
	::lforUpdate:= if(lforUpdate != nil, lforUpdate, .F.)
	
	
	// build SQL instruction	
	::cQuery:= "SELECT "+ cColumns +" FROM "+ ::getTable() + ::where(cConditional) + ::orderBy(cOrder)

	::cQuery+= if(::lforUpdate, " FOR UPDATE", "")
	
	Try
	
		if ::rddName == "SQLRDD"
		
			::dbInstance:Exec(::cQuery, .t., .t., @aResult)	
		   
		elseif ::rddName $ "ADO"
	
			oRecordSet:= ::dbInstance:Execute(::cQuery)	
     	   aResult   := oRecordSet:GetRows() 
	  
	   endif
	
	Catch oError
	
//	   cMsgErro := "Error: "  + Transform(oError:GenCode, nil) + ";" +HB_OsNewLine()+ ;
//         	 		"SubC: "   + Transform(oError:SubCode, nil) + ";" +HB_OsNewLine()+ ;
//         	 		"OSCode: "  + Transform(oError:OsCode,  nil) + ";" +HB_OsNewLine()+ ;
//         	 		"SubSystem: " + Transform(oError:SubSystem, nil) + ";" +HB_OsNewLine()+ ;
//         	 		"Mensangem: " + oError:Description

		//::ShowError()
		
	End 
	
Return aResult

********************************************************************************
Method where(cConditional) Class QueryBuilder
	
	local cExp:= if(cConditional != nil, " WHERE "+ cConditional, "")
		
Return cExp

********************************************************************************
Method orderBy(cOrder) Class QueryBuilder
	
	local cExp:= if(cOrder != nil, " ORDER BY "+ cOrder, "")
		
Return cExp

********************************************************************************
Method insert(aColumn, aData) Class QueryBuilder
	/*
	   perform an SQL Insert instruction
	   aData -> hash table containing the column name and value	
	*/
	local listColumn, listValues, oError, oRecordSet
	
	if len(aData) == 0 .OR. len(aColumn) == 0
		return .f.
	endif
	
	// transform the arrays in string list
	listColumn:= ::arrayToList( aColumn )
	listValues:= ::arrayToList( aData, ,.t.)
	
	
	// build SQL query to be performed
	::cQuery := "INSERT INTO " + ::getTable() +" ("+ listColumn +") "+;
					"VALUES ("+ listValues +")"
	
	
	// execute the SQL instuction according to the RDD used 				
	
	if ::rddName == "SQLRDD"

		SR_BeginTransaction()   

		Try
		
			::dbInstance:Exec(::cQuery) 
			
	   Catch oError

		  	SR_RollBackTransaction()
			//::ShowError()
			return .f.
	   End
	   
	elseif ::rddName $ "ADO"

   	::dbInstance:BeginTrans()
   	
   	Try
		
			oRecordSet:= ::dbInstance:Execute(::cQuery)	
		
		Catch oError
		
			::dbInstance:RollbackTrans()
		   return .f.
		End
		
		::dbInstance:CommitTrans()
		
   endif
	
Return .t.

********************************************************************************
Method update(aData) Class QueryBuilder
	// perform an SQL Update instruction	
	// param aData -> hash table with field names and its value
	
	local oValue, oKey, posKey, oRecordSet
	
			
	// build SQL query to be performed
	::cQuery := "UPDATE " + ::getTable() +" SET "

  	for i:= 1 to len(aData)
  	
  		oKey  := HGetKeyAt(aData,i)   // get the name of key
  		oValue:= HGetValueAt(aData,i) // get the value of respective key
  		
  		if oKey == ::getPrimaryKey()
  			posKey := HGetPos(aData, oKey) // obtain the position of primary key
  			loop // ignore the primary key
  		endif
		  	
  		if oValue != nil
				
			oValue:= ::cValueToSQL(oValue)
				
			::cQuery+= oKey +" = '"+ oValue + if(i >= len(aData), "'", "', ")
		   
		endif
		  	
  	next
  	
	// insert the where clausure to update the record by id
  	::cQuery+= " WHERE "+ ::getPrimaryKey() +" = '"+ cValtoStr(HGetValueAt(aData, posKey)) +"'"
	
	// execute the SQL instuction according to the RDD used 				
	
	if ::rddName == "SQLRDD"

		SR_BeginTransaction()   

		Try
		
			::dbInstance:Exec(::cQuery) 
			
	   Catch oError

		  	SR_RollBackTransaction()
			//::ShowError()
			return .f.
	   End
	   
	elseif ::rddName $ "ADO"

   	::dbInstance:BeginTrans()
   	
   	Try

			oRecordSet:= ::dbInstance:Execute(::cQuery)	

		Catch oError
		
			::dbInstance:RollbackTrans()
		   return .f.
		End
		
		::dbInstance:CommitTrans()

   endif

Return .t.

********************************************************************************
Method delete(id, cConditional) Class QueryBuilder
	// perform an SQL delete instruction
	// By default, deletes an record by identifier equals parameter id. 
	// However, it's possible to inform an condicional in second parameter	
	local oRecordSet
	
	if id == nil .AND. cConditional == nil
		return .f.
	endif
		
	::cQuery:= "DELETE FROM "+ ::getTable() 
	
	if id != nil
		::cQuery += " WHERE " + ::getPrimaryKey() +" = '"+ cValToStr(id) +"'"	
	else
		::cQuery += ::where(cConditional)
	endif
	
	if ::rddName == "SQLRDD"
	
		SR_BeginTransaction()   

		Try
		
			::dbInstance:Exec(::cQuery)	
		
		Catch oError
	
		  	SR_RollBackTransaction()
			//::ShowError()
			return .f.
		End 
	   
	elseif ::rddName $ "ADO"

   	::dbInstance:BeginTrans()
   	
   	Try

			oRecordSet:= ::dbInstance:Execute(::cQuery)	

		Catch oError
		
			::dbInstance:RollbackTrans()
		   return .f.
		End
		
		::dbInstance:CommitTrans()

   endif

Return .t.

********************************************************************************
Method leftJoin() Class QueryBuilder


Return nil

********************************************************************************
Method fullJoin() Class QueryBuilder


Return nil

********************************************************************************
Method getNewId() Class QueryBuilder
	// return new avaiable identifier from table if it's not auto increment
	local newId, nLen
	local aRecord:= {}
	
	aRecord:= ::select("Max("+ ::getPrimaryKey() +")")
	
	aRecord:= Asort(aRecord,,,{|X,Y| X[1] < Y[1]})
	
	nLen   := Len(aRecord)
	if nLen > 0 
		newId := (aRecord[nLen,1] + 1)	
	endif
	
Return newId

********************************************************************************
Method arrayToList(aArray, cDelimiter, lValue) Class QueryBuilder
	// converts an array to list with specified delimiter
	// parameter lData -> indicates that values content in aArray is field names or values
	local cList:= "", oDelim, oElement
	local lDate:= .f.
	local lFormatValues:= if(lValue == nil, .F., lValue)
	
	oDelim:= if(cDelimiter == nil, ",", cDelimiter)
	
	for i:= 1 to Len(aArray)
	
		if lFormatValues // if format values to save in database
		
		   oElement := ::cValueToSQL( aArray[i] )
			oElement := "'"+ oElement +"'" 
		   
		else
			 // means that content is equals field names
			oElement := aArray[i]	
		endif
		
		cList += oElement + if(i >= Len(aArray), "", oDelim)
	
	next
	
Return cList

********************************************************************************
Method formatDate2SQL(dData, lDateTime) Class QueryBuilder
	// format to date-time 
	local fDate
	local onlyDate:= if(lDateTime == nil, .T., lDateTime)
	
	fDate:= StrZero(Year(dData),4) +"-"+ StrZero(Month(dData),2) +"-"+ StrZero(Day(dData),2) 	

	if ! onlyDate
		// date-time formatt
		fDate += "T"+Time()	
	endif
	
return fDate

********************************************************************************
Method cValueToSQL(uData) Class QueryBuilder
	// format an data with any type to an value to insert in SQL instruction 
	local cValue
	
	if valtype(uData) == "D" 
		cValue:= ::formatDate2SQL(uData)
	else
		cValue:= cValtoStr(uData)
	endif

	cValue:= StrTran(cValue, "'", "")
	
return cValue

********************************************************************************

=======
#include "hbclass.ch"
#include "common.ch"
#include "objects.ch"
#include "database.ch"
#include "error.ch"
#include "adordd.ch"
#include "sqlrdd.ch"
#include "mysql.ch"
#include "sqlodbc.ch"

********************************************************************************
* Class : QueryBuilder from Safira ORM library - perform SQL instructions to database
* Date  : 18/04/2017
* Author: Eduardo P. Sales
********************************************************************************

Class QueryBuilder

	Data table AS String
	Data primaryKey 
	Data autoIncrement AS LOGICAL INIT .F.
	Data dbInstance
	Data cQuery AS String
	Data lforUpdate AS LOGICAL INIT .F.
	Data aStruct INIT hash()
	Data primaryKey AS String  init "id" // primary key field name
	Data rddName AS String init "SQLRDD" // rdd name
	
	Method New() CONSTRUCTOR
	
	Method setStruct()
	Method getStruct()
	
	Method setPrimaryKey()
	Method getPrimaryKey()
	
	Method getTable() 
	Method setTable()
	
	Method create() 
	Method select(strColumns, lforUpdate, cConditional, cOrder)
	Method where()
	Method orderBy()
	Method insert(aColmun, aData)
	Method update(aData)
	Method delete(id, cConditional)
	Method leftJoin()
	Method fullJoin()
	
	Method getNewId()
	Method arrayToList(aArray, cDelimiter, lValue)
	Method formatDate2SQL(dData, lDateTime)
	Method bindParams(aParams)
	Method cValueToSQL(uData)
		
	Error Handler ShowError()
		
EndClass

********************************************************************************
Method New(table, dbInstance, cRddName) Class QueryBuilder

	::table         := table
	::dbInstance    := dbInstance
	::rddName       := if(cRddName != nil, Upper(cRddName), ::rddName)
	
	::setStruct()
	
Return Self

********************************************************************************
Method setStruct() Class QueryBuilder
	// set table-model scope
	local cQuery, oRecordSet, oError, aColumn
	local aArray:= {}
	local cMsgErro:= ""
	
	if ::dbInstance == nil
		return nil
	endif

	* This SQL instruction retreive an array containing the specified table structure. Where:
	* first position = Field's name
	* second         = Data type
	* third          = if field is nullable: (YES/NO)
	* forth          = The Key Type:
	* If Key is PRI, the column is a PRIMARY KEY or is one of the columns in a multiple-column PRIMARY KEY.
	* If Key is UNI, the column is the first column of a unique-valued index that cannot contain NULL values.
	* If Key is MUL, multiple occurrences of a given value are permitted within the column. The column is the first 
	* fifth          = Default value, if exists. Ex: 0, NULL, .f., etc.
	* sixth          = Extra information about the field. Ex: Auto_increment

	cQuery:= "SHOW COLUMNS FROM "+ ::getTable()
		
	   
   Try
	
		if ::rddName == "SQLRDD"
		
			::dbInstance:Exec(cQuery, .t., .t., @aArray)	
		   
		elseif ::rddName $ "ADO"
	
			oRecordSet:= ::dbInstance:Execute(cQuery)	
         
			aArray    := oRecordSet:GetRows()
         
	   endif
      
	Catch oError

	   cMsgErro := "Error: "  + Transform(oError:GenCode, nil) + ";" +HB_OsNewLine()+ ;
         	 		"SubC: "   + Transform(oError:SubCode, nil) + ";" +HB_OsNewLine()+ ;
         	 		"OSCode: "  + Transform(oError:OsCode,  nil) + ";" +HB_OsNewLine()+ ;
         	 		"SubSystem: " + Transform(oError:SubSystem, nil) + ";" +HB_OsNewLine()+ ;
         	 		"Mensangem: " + oError:Description
   
   End
	   
	if ! empty(cMsgErro) .OR. len(aArray) == 0
		return nil
	endif
	

	// transform the array of table structure to the class attribute
	
	for i:= 1 to len(aArray)
	   
		initValue:= nil
	   aColumn  := {=>}
	   
		// define default value to the column
		if aArray[i,5] != nil
		
			initValue := aArray[i,5]
		
		else
			
			if Left(aArray[i,2],4) == 'char'

				initValue:= Space(1)
			
			elseif aArray[i,2] $ 'varchar'
			
				initValue:= Space( Substr( aArray[i,2], 9, (At(')', aArray[i,2], 9) -1) ) )
			
			elseif aArray[i,2] $ 'text'

				initValue:= Space(255)
			
			elseif aArray[i,2] $ 'int' .OR. aArray[i,2] $ 'float'
				
				initValue:= 0
			
			elseif aArray[i,2] $ 'double' .OR. aArray[i,2] $ 'decimal' .OR. aArray[i,2] $ 'real'
			
				initValue:= 0.00
			
			elseif aArray[i,2] $ 'date'
				
				initValue:= ctod("")
					
			endif

		endif
		
		// set primary key
		if aArray[i,4] == "PRI"

			::setPrimaryKey( Alltrim(aArray[i,1]) )
			
			// check if primary key is auto increment
         if aArray[i,6] == "auto_increment" 
         	::autoIncrement := .T.
         endif

      endif

	   
		aColumn:= {=>} // fields array
	   
	   for c:= 1 to 4
	   
	   	if c == 1
	   		
	   		HSet(aColumn, 'field', Alltrim(aArray[i,1])) // field's name 
	   		
	   	elseif c == 2
	   	   
				HSet(aColumn, 'nullable', aArray[i,3]) // if field is nullable
	   	   
			elseif c == 3
	   	
	   		HSet(aColumn, 'ktype', aArray[i,4]) // field key type
	   		
			elseif c == 4
	      
	      	HSet(aColumn, 'default', cValToChar(initValue)) // default value
	      	
			endif
	   next

		HSet( ::aStruct, Alltrim(Str(i)), aColumn )

	next
	
Return nil

********************************************************************************
Method getStruct() Class QueryBuilder
	// retrieve the table-model scope
Return ::aStruct

********************************************************************************
Method setPrimaryKey(oKey) Class QueryBuilder
	::primaryKey := oKey
Return nil

********************************************************************************
Method getPrimaryKey() Class QueryBuilder

Return ::primaryKey

********************************************************************************
Method getTable() Class QueryBuilder
	// return table name 
Return ::table

********************************************************************************
Method setTable(cTable) Class QueryBuilder
	// set table name 
	::table := cTable	
Return nil 

********************************************************************************
Method ShowError() Class QueryBuilder
	// error handler
	local cMsg   := __GetMessage() 
	local nError := SubStr(cMsg, 1, 1)
 
 	_ClsSetError( _GenError( nError, ::ClassName(), cMsg ) ) 
 			
Return nil
 
********************************************************************************
Method create() Class QueryBuilder
	// create new blank Model, return a hash with field names as keys
	local aRecord:= {=>}

	for i:= 1 to len(::aStruct)
		HSet(aRecord, ::aStruct[Alltrim(Str(i))]['field'], ::aStruct[Alltrim(Str(i))]['default'])
	next

return aRecord

********************************************************************************
Method select(strColumns, lforUpdate, cConditional, cOrder) Class QueryBuilder
	// perform SQL Select instruction	

	local cColumns, oRecordSet 
	local aResult:= {}
	
	cColumns    := if(strColumns != nil, strColumns, "*")
	
	::lforUpdate:= if(lforUpdate != nil, lforUpdate, .F.)
	
	
	// build SQL instruction	
	::cQuery:= "SELECT "+ cColumns +" FROM "+ ::getTable() + ::where(cConditional) + ::orderBy(cOrder)

	::cQuery+= if(::lforUpdate, " FOR UPDATE", "")
	
	Try
	
		if ::rddName == "SQLRDD"
		
			::dbInstance:Exec(::cQuery, .t., .t., @aResult)	
		   
		elseif ::rddName $ "ADO"
	
			oRecordSet:= ::dbInstance:Execute(::cQuery)	
     	   aResult   := oRecordSet:GetRows() 
	  
	   endif
	
	Catch oError
	
//	   cMsgErro := "Error: "  + Transform(oError:GenCode, nil) + ";" +HB_OsNewLine()+ ;
//         	 		"SubC: "   + Transform(oError:SubCode, nil) + ";" +HB_OsNewLine()+ ;
//         	 		"OSCode: "  + Transform(oError:OsCode,  nil) + ";" +HB_OsNewLine()+ ;
//         	 		"SubSystem: " + Transform(oError:SubSystem, nil) + ";" +HB_OsNewLine()+ ;
//         	 		"Mensangem: " + oError:Description

		//::ShowError()
		
	End 
	
Return aResult

********************************************************************************
Method where(cConditional) Class QueryBuilder
	
	local cExp:= if(cConditional != nil, " WHERE "+ cConditional, "")
		
Return cExp

********************************************************************************
Method orderBy(cOrder) Class QueryBuilder
	
	local cExp:= if(cOrder != nil, " ORDER BY "+ cOrder, "")
		
Return cExp

********************************************************************************
Method insert(aColumn, aData) Class QueryBuilder
	/*
	   perform an SQL Insert instruction
	   aData -> hash table containing the column name and value	
	*/
	local listColumn, listValues, nCnn, oError, oRecordSet
	
	if len(aData) == 0 .OR. len(aColumn) == 0
		return .f.
	endif
	
	// transform the arrays in string list
	listColumn:= ::arrayToList( aColumn )
	listValues:= ::arrayToList( aData, ,.t.)
	
	
	// build SQL query to be performed
	::cQuery := "INSERT INTO " + ::getTable() +" ("+ listColumn +") "+;
					"VALUES ("+ listValues +")"
	
	
	// execute the SQL instuction according to the RDD used 				
	
	if ::rddName == "SQLRDD"

		nCnn := SR_GetActiveConnection()

		SR_BeginTransaction()   

		Try
		
			::dbInstance:Exec(::cQuery) 
			
	   Catch oError

		  	SR_RollBackTransaction()
			//::ShowError()
			return .f.
	   End
	   
      SR_EndConnection( nCnn )   

	   
	elseif ::rddName $ "ADO"

   	::dbInstance:BeginTrans()
   	
   	Try
		
			oRecordSet:= ::dbInstance:Execute(::cQuery)	
		
		Catch oError
		
			::dbInstance:RollbackTrans()
		   return .f.
		End
		
		::dbInstance:CommitTrans()
		
   endif
	
Return .t.

********************************************************************************
Method update(aData) Class QueryBuilder
	// perform an SQL Update instruction	
	// param aData -> hash table with field names and its value
	
	local oValue, oKey, posKey, nCnn, oRecordSet
	
			
	// build SQL query to be performed
	::cQuery := "UPDATE " + ::getTable() +" SET "

  	for i:= 1 to len(aData)
  	
  		oKey  := HGetKeyAt(aData,i)   // get the name of key
  		oValue:= HGetValueAt(aData,i) // get the value of respective key
  		
  		if oKey == ::getPrimaryKey()
  			posKey := HGetPos(aData, oKey) // obtain the position of primary key
  			loop // ignore the primary key
  		endif
		  	
  		if oValue != nil
				
			oValue:= ::cValueToSQL(oValue)
				
			::cQuery+= oKey +" = '"+ oValue + if(i >= len(aData), "'", "', ")
		   
		endif
		  	
  	next
  	
	// insert the where clausure to update the record by id
  	::cQuery+= " WHERE "+ ::getPrimaryKey() +" = '"+ cValtoStr(HGetValueAt(aData, posKey)) +"'"
  	

	// execute the SQL instuction according to the RDD used 				
	
	if ::rddName == "SQLRDD"

		nCnn := SR_GetActiveConnection()

		SR_BeginTransaction()   

		Try
		
			::dbInstance:Exec(::cQuery) 
			
	   Catch oError

		  	SR_RollBackTransaction()
			//::ShowError()
			return .f.
	   End
	   
      SR_EndConnection( nCnn )   

	   
	elseif ::rddName $ "ADO"

   	::dbInstance:BeginTrans()
   	
   	Try

			oRecordSet:= ::dbInstance:Execute(::cQuery)	

		Catch oError
		
			::dbInstance:RollbackTrans()
		   return .f.
		End
		
		::dbInstance:CommitTrans()

   endif

Return .t.

********************************************************************************
Method delete(id, cConditional) Class QueryBuilder
	// perform an SQL delete instruction
	// By default, deletes an record by identifier equals parameter id. 
	// However, it's possible to inform an condicional in second parameter	
	local nCnn, oRecordSet
	
	if id == nil .AND. cConditional == nil
		return .f.
	endif
		
	::cQuery:= "DELETE FROM "+ ::getTable() 
	
	if id != nil
		::cQuery += " WHERE " + ::getPrimaryKey() +" = '"+ cValToStr(id) +"'"	
	else
		::cQuery += ::where(cConditional)
	endif
	
	if ::rddName == "SQLRDD"
	
		nCnn := SR_GetActiveConnection()

		SR_BeginTransaction()   

		Try
		
			::dbInstance:Exec(::cQuery)	
		
		Catch oError
	
		  	SR_RollBackTransaction()
			//::ShowError()
			return .f.
		End 

      SR_EndConnection( nCnn )   

	   
	elseif ::rddName $ "ADO"

   	::dbInstance:BeginTrans()
   	
   	Try

			oRecordSet:= ::dbInstance:Execute(::cQuery)	

		Catch oError
		
			::dbInstance:RollbackTrans()
		   return .f.
		End
		
		::dbInstance:CommitTrans()

   endif

Return .t.

********************************************************************************
Method leftJoin() Class QueryBuilder


Return nil

********************************************************************************
Method fullJoin() Class QueryBuilder


Return nil

********************************************************************************
Method getNewId() Class QueryBuilder
	// return new avaiable identifier from table if it's not auto increment
	local newId, nLen
	local aRecord:= {}
	
	aRecord:= ::select("Max("+ ::getPrimaryKey() +")")
	
	aRecord:= Asort(aRecord,,,{|X,Y| X[1] < Y[1]})
	
	nLen   := Len(aRecord)
	if nLen > 0 
		newId := (aRecord[nLen,1] + 1)	
	endif
	
Return newId

********************************************************************************
Method arrayToList(aArray, cDelimiter, lValue) Class QueryBuilder
	// converts an array to list with specified delimiter
	// parameter lData -> indicates that values content in aArray is field names or values
	local cList:= "", oDelim, oElement
	local lDate:= .f.
	local lFormatValues:= if(lValue == nil, .F., lValue)
	
	oDelim:= if(cDelimiter == nil, ",", cDelimiter)
	
	for i:= 1 to Len(aArray)
	
		if lFormatValues // if format values to save in database
		
		   oElement := ::cValueToSQL( aArray[i] )
			oElement := "'"+ oElement +"'" 
		   
		else
			 // means that content is equals field names
			oElement := aArray[i]	
		endif
		
		cList += oElement + if(i >= Len(aArray), "", oDelim)
	
	next
	
Return cList

********************************************************************************
Method formatDate2SQL(dData, lDateTime) Class QueryBuilder
	// format to date-time 
	local fDate
	local onlyDate:= if(lDateTime == nil, .T., lDateTime)
	
	fDate:= StrZero(Year(dData),4) +"-"+ StrZero(Month(dData),2) +"-"+ StrZero(Day(dData),2) 	

	if ! onlyDate
		// date-time formatt
		fDate += "T"+Time()	
	endif
	
return fDate

********************************************************************************
Method bindParams(aParams) Class QueryBuilder
	// used to bind the parameters to execute insert or update sql instructions
	// return a list containing the parameters binded
	
	local listBinded:= ""
		
	for i:= 1 to len(aParams)
		listBinded += "?" + if(i>= len(aParams), "", ", ")
	next
	
Return listBinded

********************************************************************************
Method cValueToSQL(uData) Class QueryBuilder
	// format an data with any type to an value to insert in SQL instruction 
	local cValue
	
	if valtype(uData) == "D" 
		cValue:= ::formatDate2SQL(uData)
	else
		cValue:= cValtoStr(uData)
	endif

	cValue:= StrTran(cValue, "'", "")
	
return cValue

********************************************************************************

>>>>>>> 5033576189c5fdb18b55c0d8fec49d73ac1cd30a
