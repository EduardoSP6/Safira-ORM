#include "hbclass.ch"
#include "common.ch"
#include "objects.ch"
#include "error.ch"

********************************************************************************
* Class : Model from Safira ORM library
* Date  : 16/04/2017
* Author: Eduardo P. Sales
********************************************************************************

Class Model

	Data table
	Data aScope INIT hash()    // array of table model fields
	Data dbInstance AS Object  // connection instance object
	Data modelName  AS String  // Class name of model
	Data rddName    AS String  init "SQLRDD"
	Data objModel   AS Object
	Data objQuery   AS Object
	Data lautoInc   AS Logical init .F.
	 
	Method New() Constructor
	
	Method setModel() 
	Method getModel()
	
	Method getTable() 
	Method setTable(cTableName) 

	Method all(cOrder)
	Method find(id) 
	Method create()
	Method save(aData)
	Method update(aData)
	Method delete(id)
	
	Method getFieldsName(lIgnoreId)
	
EndClass

********************************************************************************
Method New(oModel, tableName, conInstance, cRddName) Class Model
	
	// build instance of object
	
	::rddName:= if(cRddName != nil, Upper(cRddName), ::rddName) // indicate the RDD Name
	
	::objModel   := oModel // instance of mapped model class
	::modelName  := oModel:ClassName() // name of model class
	::dbInstance := conInstance  // instance of database connection object

	::setTable( tableName ) // set table mapped name
	
	// create new instance of QueryBuilder 
	::objQuery:= QueryBuilder():New(tableName, conInstance, ::rddName)
	                                 
	::aScope  := ::objQuery:getStruct() // obtain the table structure
	
	::lautoInc:= ::objQuery:autoIncrement // property that indicates if primary key is auto increment
	
Return Self

********************************************************************************
Method getModel() Class Model
	// retrieve the model object
Return ::objModel	

********************************************************************************
Method setModel(oModel) Class Model
	// set model object
	::objModel:= oModel
Return nil		

********************************************************************************
Method getTable() Class Model
	// retrieve table associated with model	
Return ::table

********************************************************************************
Method setTable(cTableName) Class Model
	// set table name to the model
	::table := cTableName	

Return nil

********************************************************************************
Method all(cOrder) Class Model
	// retrieve an array of all records from the model table

	local aArray:= {}
	
	if ::objQuery == nil
		return aArray
	endif
	
	aArray:= ::objQuery:select(,,, cOrder)	
			
Return aArray

********************************************************************************
Method find(id) Class Model
	// find a record by it's identifier and return a hash table

	local oKey, aRecord, oError, oRecordSet, cMsgErro:= cQuery:= ""
	local aArray:= {}

	oKey:= ::objQuery:getPrimaryKey()  // recover the primary key
	
	// execute sql select
	aArray:= ::objQuery:select( nil, .f., (lower(oKey)+" = '"+ cValtoChar(id) +"'") )	

	// convert the array returned by sql select to an hash table
	aRecord:= {=>}
	if len(aArray) > 0
		for i:= 1 to len(aArray)
			for x:= 1 to len(::aScope)
			
				HSet( aRecord, ::aScope[Alltrim(Str(x))]['field'], cValtoChar(aArray[i,x]) )
			   
			next
		next
	endif

Return aRecord

********************************************************************************
Method create() Class Model
	// create new blank record in hash table mode	
Return ::objQuery:create()

********************************************************************************
Method save(aData) Class Model
	// save the model in respective database
	// aData -> hash table with field names as keys and it content as values
	
	local aKeys  := {}
	local aValues:= {}
	local oId, nPos
	
	if len(aData) == 0 .OR. valtype(aData) != "H"
		return .f.
	endif	
		
	// obtain the position of primary key in hash table
	nPos:= HGetPos(aData, ::objQuery:getPrimaryKey())
	if nPos == 0
   	return .f.
	endif
	
	// if identifier does not an auto increment key, return an avaiable id for the record
	if ! ::lautoInc
		
		oId:= ::objQuery:getNewId()

		if oId == nil
			return .f.
		endif	                                          
		
		// update the hash value with the new id
		HSetValueAt(aData, nPos, oId)
		
	else
		// if the id is auto increment, remove the id from the hash table
		HDelAt(aData, nPos)			
	endif
	
	// explode the hash table in 2 arrays. One containing the keys and other, the values.
	
	for i:= 1 to len(aData)
	
		Aadd(aKeys  , HGetKeyAt(aData, i))
		Aadd(aValues, HGetValueAt(aData, i)) 
		
	next
	
	// initiate the transaction with database an returns logical value if success
Return ::objQuery:insert(aKeys, aValues)

********************************************************************************
Method update(aData) Class Model
	// update a record from table model	

	if valtype(aData) != "H" .OR. len(aData) == 0
		return .f.
	endif
		
Return ::objQuery:update(aData)

********************************************************************************
Method delete(id) Class Model
	// delete a record from table model
Return ::objQuery:delete(id)

********************************************************************************
Method getFieldsName(lIgnoreId) Class Model
	// return an array with fields name from the mapped table

	local aField:= {}
	
	for i:= 1 to len(::aScope)
	   
		// ignore the primary key according to the parameter
	   if lIgnoreId .AND. ::aScope[Alltrim(Str(i))]['field'] == ::getPrimaryKey()
	   	loop
	   endif	
	   
		AADD(aField, ::aScope[Alltrim(Str(i))]['field'])
		
	next
	
Return aField

********************************************************************************