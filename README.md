# mod_mam_export

  Export MAM (XEP-0313) archive preferences and messages from internal mnesia DB to MySQL DB

Description
-----------

Export MAM archive preferences and messages from internal mnesia DB to MySQL DB. Depending on the size of your database, this might take considerable amount of time.

Prerequisites
-------------

You should have the MySQL database configured in your configuration. If not please refer to the documentation on using MySQL with ejabberd [here](https://docs.ejabberd.im/admin/guide/databases/mysql/)

Configuration
-------------------

Add the module to your ejabberd.yml, on the modules section:
```yml
  modules:
    mod_mam_export: {}
```
Ejabberd Command
----------------

You can execute the command using ejabberdctl:
```yml
export_mam_archive host
export_mam_archive_prefs host
```
Parameters:
	host - Your server host

You can check the logs for the list of messages being inserted.
