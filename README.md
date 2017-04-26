# Safira-ORM for xHarbour

Safira ORM is a kind of microframework for xHarbour, with the aim of help xHarbour developers. 
Based on the principles of relational object mapping (ORM), makes the developers have more productivity
in the development of their software, since it is not necessary to worry so much about transactions with the database.

It has RDD support: SqlRdd and ADO, for a while.
It works with hash tables in the 90% to manipulate the data.
Easy to integrate with your software.


Integration:

In your model class, create an data like bellow to save the Model Object. After, in the constructor method,
build an instance of Model Class passing the instance of current object (CUSTOMER), the name of table, the instance of
database connection and at last, the name of RDD.

After that, we have to create an method responsable to recover the Model Object. And it's done! :)

Example of use:

Class Customer
  
  DATA oModel    AS Object
  ...
EndClass

*****************************************************************

Method New(oDriver) Class Customer
	* constructor method - dependency injection from ORM Model
	
	::oModel:= Model():New(Self, 'tb_caixa', oDriver, "SQLRDD")
	
Return Self

*****************************************************************
Method toModel() Class Customer
	* return model to access those methods externally
Return ::oModel

*****************************************************************


Executing transactions with the database and obtaining the results:

Now we are ready to execute sql instructions with database. The class that is responsable to do it, is called QueryBuilder.
So, in Controller file of your project you can call these methods bellow and check the results. 

Example using SQLRDD:

* make an instance of class Customer, passing in constructor method, an instance of the database connection object. 
* To inject the dependencies of Model Class, as shown above.

oCustomer:= Customer():New(SR_GetConnection())

* Now we can call any of Methods avaiable: all, find, create, save, update and delete. Using the method that
* we have created in the Customer model class.


* Method all, retrieve all records from mapped table in an two-dimensional array
oCustomer:toModel():all() 

* Method create, retrieve an hash table containing as keys the fields name of table, with blank values.
oCustomer:toModel():create() 

* Method find, returns a hash table with the record found in database according to the id passed by parameter.
oCustomer:toModel:find(id) 

*	Method save, insert in table a new record. The parameter is an hash table having the same structure that create method returns.
oCustomer:toModel:save(aHash) 
	
*	This method updates an record in database. The parameter is an hash table having the same structure that create method returns.
oCustomer:toModel:update(aHash) 

* This method delete an record in database if found by id.
oCustomer:toModel:delete(id) 













