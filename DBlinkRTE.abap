*&---------------------------------------------------------------------*
*& Report  ZICM_DBLINK_TABLES_EXPLORER
*&
*&---------------------------------------------------------------------*
*& ConVista, Glushchenko V.
*&---------------------------------------------------------------------*

report  zicm_dblink_tables_explorer.

type-pools slis.
type-pools icon.

*******************************************************************************
* CONSTANTS
*******************************************************************************
constants lc_dbms_ora type dbcon_dbms value 'ORA'.
constants lc_ucomm_dblclk type sy-ucomm value '&IC1'.
constants lc_dbcon_default type dbcon-con_name value cl_sql_connection=>c_default_connection.
constants lc_abap_scheme type string value 'SAPSR3'. " Default scheme used by SAP instances

*******************************************************************************
* TYPES
*******************************************************************************

*----------------------------------------------------------------------*
* Интерфейс для работы c данными в другой системе через DBLink
*----------------------------------------------------------------------*
interface lif_dblink.
  types : begin of ty_dbsize
        ,  owner type string
        ,  name  type string
        ,  rows  type int4
        , end of ty_dbsize
        , tt_dbsize type standard table of ty_dbsize
        .

  types : begin of ts_proc
        ,  owner          type string
        ,  object_name    type string
        ,  procedure_name type string
        ,  object_type    type string
        , end of ts_proc
        , tt_proc type standard table of ts_proc

        , begin of ts_proc_source
        ,  owner        type string
        ,  name         type string
        ,  type         type string
        ,  line         type i
        ,  text         type string
        , end of ts_proc_source
        , tt_proc_source type standard table of ts_proc_source

        , begin of ts_sql_statement
*          ,  owner        type string
*          ,  name         type string
*          ,  object_name  type string
*          ,  text         type string
        ,  sql_text type string
        , end of ts_sql_statement
        , tt_sql_statement type standard table of ts_sql_statement

        , begin of ts_constraint
        ,  constraint_name  type char30
        ,  constraint_type  type char1
        ,  table_name       type char30
        ,  search_condition type string
        ,  r_constraint_name type char30
        ,  delete_rule      type char9
        ,  validated        type char13
        ,  index_name       type char30
        , end of ts_constraint
        , tt_constraint type standard table of ts_constraint

        , begin of ty_sql_constraints
        ,  column_name     type c length 100
        ,  constraint_name type c length 100
        ,  position        type i
        , end of ty_sql_constraints

        , begin of ty_columns
        ,  column     type string
        ,  type       type string
        ,  length     type i
        ,  constraint type string
        , end of ty_columns
        , tt_columns type standard table of ty_columns

*        , begin of ty_sql_columns
*        ,  column type c length 100
*        ,  type   type c length 60
*        ,  length type i
*        , end of ty_sql_columns
        .
  methods set_dbs importing iv_dbs type dbcon_name.
  methods get_dbs returning value(ev_dbs) type dbcon_name.
  methods set_scheme importing iv_scheme type string.
  methods get_scheme returning value(ev_scheme) type string.


  methods fetch_db_tables exporting et_dbsize type tt_dbsize.
  methods fetch_table_columns importing iv_tabname type string
                              exporting et_columns type tt_columns.
  methods fetch_constraint importing iv_constraint_name type string
                           exporting et_constraint      type tt_constraint.
  methods fetch_table_data importing iv_tabnm   type string
                                     it_columns type tt_columns
                           exporting ed_data    type ref to data.
  methods fetch_procs exporting et_procs type tt_proc.
  methods fetch_procs_source importing iv_objname  type string
                                       iv_procname type string
                             exporting et_source   type tt_proc_source.
  methods fetch_sql exporting et_sql type tt_sql_statement.
  methods skip_scheme_mode importing iv_skip type abap_bool.
  methods is_skip_scheme returning value(rv_skip) type abap_bool.
endinterface.

*----------------------------------------------------------------------*
*       CLASS lcl_appl DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class lcl_appl definition.
  public section.
    types : ty_mode type i .
    constants : c_mode_table type ty_mode value 1   " Tables/Views
              , c_mode_procs type ty_mode value 2   " Stored procs
              , c_mode_sql   type ty_mode value 3   " SQL
              .
    data : mt_dbsize type lif_dblink=>tt_dbsize .
    data : mt_procs  type lif_dblink=>tt_proc .

    class-methods f4_scheme.
    class-methods get_con_user importing iv_dbcon        type dbcon_name
                               returning value(rv_uname) type dbcon_uid.
    methods get_db_type importing iv_dbs         type dbcon-con_name
                        returning value(rv_type) type dbcon_dbms.
    methods set_mode importing iv_mode type ty_mode.
    methods main.

    methods get_dblink returning value(ro_link) type ref to lif_dblink.
  protected section.
    data : mv_mode  type ty_mode value c_mode_table
         , mo_dblink type ref to lif_dblink
         .

endclass.
*----------------------------------------------------------------------*
*       CLASS lcl_gui DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class lcl_gui definition.
  public section.
    class-methods progress importing iv_prcnt type i
                                     iv_msg   type string.
endclass.                    "lcl_gui DEFINITION

*----------------------------------------------------------------------*
* Класс для работы c данными в другой системе через DBLink
*----------------------------------------------------------------------*
class lcl_dblink_oracle definition .
  public section.

    interfaces lif_dblink.

    aliases : set_dbs for lif_dblink~set_dbs
            , get_dbs for lif_dblink~get_dbs
            , set_scheme for lif_dblink~set_scheme
            , get_scheme for lif_dblink~get_scheme
            , fetch_db_tables for lif_dblink~fetch_db_tables
            , fetch_table_columns for lif_dblink~fetch_table_columns
            , fetch_table_data for lif_dblink~fetch_table_data
            , fetch_constraint for lif_dblink~fetch_constraint
            , fetch_procs for lif_dblink~fetch_procs
            , fetch_procs_source for lif_dblink~fetch_procs_source
            , fetch_sql for lif_dblink~fetch_sql
            , skip_scheme_mode for lif_dblink~skip_scheme_mode
            , is_skip_scheme for lif_dblink~is_skip_scheme
            .
  private section.
    data : mv_dbs    type dbcon_name
         , mv_scheme type string
         , mv_skip_scheme type abap_bool.

endclass.                    "lcl_dbview_processor DEFINITION

*----------------------------------------------------------------------*
* Класс для визуализации списка таблиц в ALV Grid'е
* Через функционал ФМ REUSE_ALV_GRID_DISPLAY
*----------------------------------------------------------------------*
class lcl_alv_tables_ui definition.
  public section.
    methods constructor importing io_dblink type ref to lif_dblink.
    methods show_alv.
    methods build_fieldcat changing o_columns type ref to cl_salv_columns.
    methods dbl_click for event double_click of cl_salv_events_table
      importing row column.
    methods on_click_toolbar for event added_function of cl_salv_events
      importing e_salv_function.
  protected section.
    constants lc_ucomm_ctable type sy-ucomm value 'CUST_TABLE'.
    data : mo_dblink type ref to lif_dblink.
endclass.                    "lcl_alv_tables_ui DEFINITION

*----------------------------------------------------------------------*
* Класс для визуализации исходных текстов для хранимых процедур
*----------------------------------------------------------------------*
class lcl_proc_source_ui definition.
  public section.
    methods constructor importing io_dblink type ref to lif_dblink.
    methods show importing iv_objname  type string
                           iv_procname type string.
*    methods set_table_name importing iv_tabnm type string.
*    methods get_table_name returning value(ev_tabnm) type string.
*    methods on_click_toolbar for event added_function of cl_salv_events
*      importing e_salv_function.
    methods free.
  private section.
    data : mo_dblink    type ref to lif_dblink
         , lv_proc      type string
         , mo_dock      type ref to cl_gui_docking_container
         , mo_editor    type ref to cl_gui_textedit
         .
endclass.                    "lcl_salv_columns_ui DEFINITION


*----------------------------------------------------------------------*
* Класс для визуализации списка процедур в ALV Grid'е
* Через функционал ФМ REUSE_ALV_GRID_DISPLAY
*----------------------------------------------------------------------*
class lcl_alv_procs_ui definition.
  public section.
    methods constructor importing io_dblink type ref to lif_dblink.
    methods show_alv.
    methods build_fieldcat changing o_columns type ref to cl_salv_columns.
    methods dbl_click for event double_click of cl_salv_events_table
      importing row column.
  protected section.
    data : mo_dblink type ref to lif_dblink
         , mo_proc_source_ui type ref to lcl_proc_source_ui
         .
endclass.                    "lcl_alv_procs_ui DEFINITION

*----------------------------------------------------------------------*
* Класс для визуализации данных из таблицы через ALV Grid
*----------------------------------------------------------------------*
class lcl_salv_table_data_ui definition.
  public section.
    methods constructor importing io_dblink type ref to lif_dblink.
    methods show_alv importing iv_tabname type string optional
                               it_columns type lif_dblink=>tt_columns optional.
    methods build_fieldcat importing io_columns type ref to cl_salv_columns_table.
  private section.
    data : mo_dblink  type ref to lif_dblink
         , lv_tabname type string
         , lt_columns type lif_dblink=>tt_columns
         , ld_tabcont type ref to data
         .
endclass.                    "lcl_salv_table_data_ui DEFINITION


*----------------------------------------------------------------------*
* Класс для визуализации списка полей для конкретной таблицы
* через ALV Grid
*----------------------------------------------------------------------*
class lcl_salv_columns_ui definition.
  public section.
    methods constructor importing io_dblink type ref to lif_dblink.
    methods show_alv.
    methods build_fieldcat importing io_columns type ref to cl_salv_columns.
    methods set_table_name importing iv_tabnm type string.
    methods get_table_name returning value(ev_tabnm) type string.
    methods on_click_toolbar for event added_function of cl_salv_events
      importing e_salv_function.
    methods free.
  private section.
    constants : lc_ucomm_tablecont  type sy-ucomm value 'TABLECONT'
              , lc_ucomm_ztable     type sy-ucomm value 'ZTABLE'
              .
    data : mo_dblink    type ref to lif_dblink
         , lv_tabnm     type string
         , lt_columns   type lif_dblink=>tt_columns
         , lo_dock      type ref to cl_gui_docking_container
         , lo_salv      type ref to cl_salv_table
         , mo_alv_table_data_ui type ref to lcl_salv_table_data_ui
         .
endclass.                    "lcl_salv_columns_ui DEFINITION
*******************************************************************************
* GLOBAL DATA
*******************************************************************************
data lv_msg type string.

data go_alv_columns_ui type ref to lcl_salv_columns_ui.

*******************************************************************************
* SCREENS
*******************************************************************************
selection-screen begin of block b01 with frame title desc.
parameters p_dbs type dbcon-con_name default lc_dbcon_default obligatory.
parameters p_scheme type char30 default 'RGSSCC' obligatory.
parameters p_nschem type abap_bool as checkbox default abap_false.
selection-screen end of block b01.

tables sscrfields.
selection-screen: function key 1."Tables
"selection-screen: function key 2."Views
selection-screen: function key 3."Stored procs
"selection-screen: function key 4."SQL
selection-screen: function key 5."Get connection user

initialization.
  data(go_appl) = new lcl_appl(  ).
  desc = 'Explore tables'.
  sscrfields-functxt_01 = value smp_dyntxt( icon_id = icon_database_table icon_text = 'Table' ).
  sscrfields-functxt_02 = value smp_dyntxt( icon_id = icon_wd_views       icon_text = 'Views' ).
  sscrfields-functxt_03 = value smp_dyntxt( icon_id = icon_oo_method      icon_text = 'Stored procs' ).
  sscrfields-functxt_04 = value smp_dyntxt( icon_id = icon_query          icon_text = 'SQL' ).
  sscrfields-functxt_05 = value smp_dyntxt( text = 'User' quickinfo = 'Get connection user' ).

at selection-screen on value-request for p_scheme.
  lcl_appl=>f4_scheme( ).


at selection-screen .
  case sy-ucomm.
    when 'FC01'.
      desc = 'Explore tables'.
      go_appl->set_mode( lcl_appl=>c_mode_table ).
    when 'FC02'.
      desc = 'Explore views'.
      go_appl->set_mode( lcl_appl=>c_mode_table ).
    when 'FC03'.
      desc = 'Explore stored procs'.
      go_appl->set_mode( lcl_appl=>c_mode_procs ).
    when 'FC04'.
      desc = 'Explore SQLs'.
      go_appl->set_mode( lcl_appl=>c_mode_sql ).
    when 'FC05'.
      p_scheme = go_appl->get_con_user( iv_dbcon = p_dbs ).

  endcase.

*******************************************************************************
* MAIN CODE
*******************************************************************************
start-of-selection.

  if p_dbs = lc_dbcon_default and p_scheme <> lc_abap_scheme.
    p_scheme = lc_abap_scheme.

    lv_msg = |For { p_dbs } connection will be used scheme { p_scheme }|.
    message lv_msg type 'I' display like 'W'.
  endif.

  if go_appl->get_db_type( p_dbs ) = lc_dbms_ora or p_dbs = lc_dbcon_default.  " ORACLE or we used DEFAULT scheme
    go_appl->main( ).
  else.
    lv_msg = |Unsupported DB connection|.
    message lv_msg type 'I' display like 'W'.
  endif.

  exit.


*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class lcl_appl implementation.
  method get_con_user.
    if iv_dbcon = cl_sql_connection=>c_default_connection.
      rv_uname = lc_abap_scheme.
      return.
    endif.
    select single user_name
      into rv_uname
      from dbcon
      where con_name = iv_dbcon.
  endmethod.
  method f4_scheme.
    types : begin of t_scheme
          ,   scheme type char30
          , end of t_scheme
          , tt_scheme type standard table of t_scheme
          .

    data : lv_search     type string
         , lt_dynpfields type table of dynpread
         , lt_schemes    type tt_scheme
         , lv_dbs        like p_dbs
         , lt_mapping    type standard table of dselc
         .

    call function 'DYNP_VALUES_READ'
      exporting
        dyname               = sy-repid
        dynumb               = sy-dynnr
        request              = 'A'
      tables
        dynpfields           = lt_dynpfields
      exceptions
        invalid_abapworkarea = 1
        invalid_dynprofield  = 2
        invalid_dynproname   = 3
        invalid_dynpronummer = 4
        invalid_request      = 5
        no_fielddescription  = 6
        invalid_parameter    = 7
        undefind_error       = 8
        double_conversion    = 9
        stepl_not_found      = 10
        others               = 11.

    lv_search = lt_dynpfields[ fieldname = 'P_SCHEME' ]-fieldvalue.
    if lv_search cs '*'.
      replace '*' with '%' into lv_search.
    else.
      clear lv_search.
    endif.
    lv_dbs = lt_dynpfields[ fieldname = 'P_DBS' ]-fieldvalue.

    "-------------------------------------------
    " get list of schemas from connection
    "-------------------------------------------
    try.
        data(lo_con) = cl_sql_connection=>get_connection( exporting con_name = lv_dbs ).
        data(lo_sql) = new cl_sql_statement( con_ref = lo_con ).
        data(lv_sql) = |select username as schema_name from sys.all_users|.
        if lv_search is not initial.
          lv_sql = |{ lv_sql } where username like '{ lv_search }'|.
        endif.
        lv_sql = |{ lv_sql } order by username|.

        data(lo_result) = lo_sql->execute_query( statement = lv_sql ).
        lo_result->set_param_table( exporting itab_ref = ref #( lt_schemes ) ).
        lo_result->next_package( ).
      catch cx_sql_exception.    "
      catch cx_parameter_invalid.    "
    endtry.

    lt_mapping = value #( ( fldname = 'F0001'   dyfldname = 'P_SCHEME' ) ).


    call function 'F4IF_INT_TABLE_VALUE_REQUEST'
      exporting
        retfield        = 'SCHEME'
        dynpprog        = sy-repid
        dynpnr          = sy-dynnr
        dynprofield     = 'P_SCHEME'
        value_org       = 'S'
      tables
        value_tab       = lt_schemes
        dynpfld_mapping = lt_mapping.
  endmethod.

  method get_db_type.
    select single dbms
           from dbcon
           into rv_type
           where con_name = iv_dbs.
  endmethod.

  method main.
    " определение списка таблиц и полей прорабатывалось только для БД ORACLE
    mo_dblink = new lcl_dblink_oracle( ).
    mo_dblink->set_dbs( p_dbs ).
    mo_dblink->set_scheme( conv #( p_scheme ) ).
    mo_dblink->skip_scheme_mode( iv_skip = p_nschem ).

    case mv_mode.
      when c_mode_table.
        data(go_alv_tables_ui) = new lcl_alv_tables_ui( mo_dblink ).
        go_alv_tables_ui->show_alv( ).
      when c_mode_procs.
        data(go_alv_procs_ui) = new lcl_alv_procs_ui( mo_dblink ).
        go_alv_procs_ui->show_alv(  ).
      when c_mode_sql.
        mo_dblink->fetch_sql(
*          importing
*            et_sql =
        ).
    endcase.
  endmethod.

  method set_mode.
    mv_mode = iv_mode.
  endmethod.

  method get_dblink.
    ro_link = mo_dblink.
  endmethod.
endclass.
*----------------------------------------------------------------------*
* Класс агрегирует функционал для упрощенной работы с SAPGUI
*----------------------------------------------------------------------*
class lcl_gui implementation.

  method progress.
    check iv_msg ne space.
    call function 'SAPGUI_PROGRESS_INDICATOR'
      exporting
        percentage = iv_prcnt
        text       = iv_msg.
  endmethod.                    "progress

endclass.                    "lcl_gui IMPLEMENTATION

*----------------------------------------------------------------------*
* Класс для работы данными в другой системе через DBLink
*----------------------------------------------------------------------*
class lcl_dblink_oracle implementation.

  method set_dbs.
    mv_dbs = iv_dbs.
  endmethod.                    "set_dbs
  method get_dbs.
    ev_dbs = mv_dbs.
  endmethod.                    "get_dbs
  method set_scheme.
    mv_scheme = iv_scheme.
  endmethod.                    "set_scheme
  method get_scheme.
    ev_scheme = mv_scheme.
  endmethod.                    "get_scheme
  method skip_scheme_mode.
    mv_skip_scheme = iv_skip.
  endmethod.
  method is_skip_scheme.
    rv_skip = mv_skip_scheme.
  endmethod.
  method fetch_db_tables.

    try.

        case mv_skip_scheme.
          when abap_true.
            data(lv_sql) = |SELECT owner, table_name, num_rows FROM all_all_tables|.
          when abap_false.
            lv_sql = |SELECT owner, table_name, num_rows FROM all_all_tables WHERE owner = '{ mv_scheme }'|.
        endcase.

        data(lo_con) = cl_sql_connection=>get_connection( con_name = mv_dbs
                                                          sharable = abap_true ).
        data(lo_sql) = new cl_sql_statement( con_ref = lo_con ).
        data(lo_res) = lo_sql->execute_query( statement = lv_sql ).
        lo_res->set_param_table( itab_ref = ref #( et_dbsize ) ).
        lo_res->next_package( ).
        lo_res->close( ).
        lo_con->close( ).

      catch cx_sql_exception into data(lx_sql).
        message lx_sql->get_longtext( ) type 'E'.

      catch cx_parameter_invalid_type into data(lx_parameter_type).
        message lx_parameter_type->get_longtext( ) type 'E'.

      catch cx_parameter_invalid into data(lx_parameter).
        message lx_parameter->get_longtext( ) type 'E'.

    endtry.

  endmethod.                    "fetch_db_tables

  method fetch_table_columns.

    data lwa_sql_constraints type lif_dblink=>ty_sql_constraints.
    field-symbols <column> type lif_dblink=>ty_columns.

    data lv_cpos type c length 5.

    data lo_connection type ref to cl_sql_connection.
    data lo_sql type ref to cl_sql_statement.
    data lo_result type ref to cl_sql_result_set.
    data lo_err type ref to cx_sql_exception.
    data ld_dref type ref to data.

    data lv_sql type string.

    try.
        lo_connection = cl_sql_connection=>get_connection( p_dbs ).
        create object lo_sql
          exporting
            con_ref = lo_connection.
      catch cx_sql_exception into lo_err.
        message lo_err type 'I' display like 'E'.
        return. " exit from procedure
    endtry.

    case mv_skip_scheme.
      when abap_true.
        lv_sql = |SELECT column_name, data_type, char_length, ' ' FROM all_tab_columns WHERE table_name = '{ iv_tabname }'|.
      when abap_false.
        lv_sql = |SELECT column_name, data_type, char_length, ' ' FROM all_tab_columns WHERE owner = '{ mv_scheme }' and table_name = '{ iv_tabname }'|.
    endcase.


    " получаем данные через DB Link
    get reference of et_columns into ld_dref.
    try.
        lo_result = lo_sql->execute_query( lv_sql ).
        lo_result->set_param_table( ld_dref ).
        lo_result->next_package( ).
      catch cx_sql_exception into lo_err.
        message lo_err type 'I' display like 'E'.
    endtry.

    lv_sql = |SELECT column_name, constraint_name, position FROM all_cons_columns WHERE table_name = '{ iv_tabname }'|.

    " получаем данные через DB Link
    get reference of lwa_sql_constraints into ld_dref.
    try.
        lo_result = lo_sql->execute_query( lv_sql ).
        lo_result->set_param_struct( ld_dref ).
        while lo_result->next( ) > 0.
          read table et_columns with key column = lwa_sql_constraints-column_name assigning <column>.
          if <column> is assigned.
            write lwa_sql_constraints-position to lv_cpos.
            concatenate <column>-constraint lwa_sql_constraints-constraint_name lv_cpos ';' into <column>-constraint separated by space.
*            fetch_constraint( exporting iv_constraint_name = conv #( lwa_sql_constraints-constraint_name )
*                              importing et_constraint      = data(lt_const) ).
            "<column>-constraint = lwa_sql_constraints-position.
          endif.
        endwhile.
      catch cx_sql_exception into lo_err.
        message lo_err type 'I' display like 'E'.
    endtry.

  endmethod.
  method fetch_constraint.
    data lv_sql type string.

    try.

        case mv_skip_scheme.
          when abap_true.
            lv_sql = |SELECT constraint_name, constraint_type, table_name, search_condition, r_constraint_name, delete_rule, validated, index_name | &&
                     | FROM all_constraints WHERE constraint_name = '{ iv_constraint_name }'|.
          when abap_false.
            lv_sql = |SELECT constraint_name, constraint_type, table_name, search_condition, r_constraint_name, delete_rule, validated, index_name | &&
                     | FROM all_constraints WHERE owner = '{ mv_scheme }' and constraint_name = '{ iv_constraint_name }'|.
        endcase.


        data(lo_con) = cl_sql_connection=>get_connection( con_name = mv_dbs
                                                          sharable = abap_false ).
        data(lo_sql) = new cl_sql_statement( con_ref = lo_con ).
        data(lo_res) = lo_sql->execute_query( statement = lv_sql ).
        lo_res->set_param_table( itab_ref = ref #( et_constraint ) ).
        lo_res->next_package( ).
        lo_res->close( ).
        lo_con->close( ).

      catch cx_sql_exception into data(lx_sql).
        message lx_sql->get_longtext( ) type 'E'.

      catch cx_parameter_invalid_type into data(lx_parameter_type).
        message lx_parameter_type->get_longtext( ) type 'E'.

      catch cx_parameter_invalid into data(lx_parameter).
        message lx_parameter->get_longtext( ) type 'E'.

    endtry.

  endmethod.
  method fetch_table_data.

    data lo_connection type ref to cl_sql_connection.
    data lo_sql type ref to cl_sql_statement.
    data lo_result type ref to cl_sql_result_set.
    data lo_err type ref to cx_sql_exception.
    data lv_sql type string.
    data lv_fields type string.
    field-symbols <columns> type lif_dblink=>ty_columns.

    " готовим SQL запрос
    case mv_skip_scheme.
      when abap_true.
        lv_sql = 'SELECT %fields% FROM %table%'.
      when abap_false.
        lv_sql = 'SELECT %fields% FROM %scheme%.%table%'.
    endcase.


    loop at it_columns assigning <columns>.
      concatenate lv_fields ',' <columns>-column into lv_fields.
    endloop.
    shift lv_fields by 1 places left.

    replace '%fields%' with lv_fields into lv_sql.
    replace '%table%' with iv_tabnm into lv_sql.
    replace '%scheme%' with mv_scheme into lv_sql.

    " готовим таблицу для результирующих данных с нужным набором полей
    data lt_fieldcatalog type lvc_t_fcat.
    data ls_fieldcatalog type lvc_s_fcat.
    data lr_dyn_tab type ref to data.

    loop at it_columns assigning <columns>.
      ls_fieldcatalog-fieldname = <columns>-column.
      ls_fieldcatalog-datatype  = 'CHAR'.
      ls_fieldcatalog-inttype   = 'C'.
      ls_fieldcatalog-intlen    = '100'.
      ls_fieldcatalog-lowercase = 'X'.
      ls_fieldcatalog-reptext   = <columns>-column.
      append ls_fieldcatalog to lt_fieldcatalog.
    endloop.

    call method cl_alv_table_create=>create_dynamic_table
      exporting
        it_fieldcatalog           = lt_fieldcatalog
      importing
        ep_table                  = lr_dyn_tab
      exceptions
        generate_subpool_dir_full = 1
        others                    = 2.

    " получаем данные через DB Link
    try.
        lo_connection = cl_sql_connection=>get_connection( p_dbs ).
        lo_sql = new #( con_ref = lo_connection ).

        lo_result = lo_sql->execute_query( lv_sql ).
        lo_result->set_param_table( lr_dyn_tab ).
        if lo_result->next_package( ) > 0.
          ed_data = lr_dyn_tab.
        endif.
      catch cx_sql_exception into lo_err.
        message lo_err type 'I' display like 'E'.
    endtry.

  endmethod.                    "fetch_table_data

  method fetch_procs.
    data lv_sql type string.

    "-------------------------------------------
    " get list of schemas from connection
    "-------------------------------------------
    case mv_skip_scheme.
      when abap_true.
        lv_sql = |SELECT distinct owner, object_name, procedure_name, object_type FROM all_procedures|.
      when abap_false.
        lv_sql = |SELECT distinct owner, object_name, procedure_name, object_type FROM all_procedures WHERE owner = '{ mv_scheme }'|.
    endcase.

    try.
        data(lo_con) = cl_sql_connection=>get_connection( exporting con_name = p_dbs ).
        data(lo_sql) = new cl_sql_statement( con_ref = lo_con ).
        data(lo_result) = lo_sql->execute_query( statement = lv_sql ).
        lo_result->set_param_table( exporting itab_ref = ref #( et_procs ) ).
        lo_result->next_package( ).
      catch cx_sql_exception.    "
        " NOP
      catch cx_parameter_invalid.    "
        " NOP
    endtry.
  endmethod.

  method fetch_procs_source.
    data lv_sql type string.

    "-------------------------------------------
    " get list of schemas from connection
    "-------------------------------------------
    case mv_skip_scheme.
      when abap_true.
        lv_sql = |SELECT owner, name, type, line, text FROM all_source_ae s2 WHERE name = '{ iv_objname }'|.
      when abap_false.
        lv_sql = |SELECT owner, name, type, line, text FROM all_source_ae s2 WHERE name = '{ iv_objname }' AND owner = '{ mv_scheme }'|.
    endcase.

    try.
        data(lo_con) = cl_sql_connection=>get_connection( exporting con_name = p_dbs ).
        data(lo_sql) = new cl_sql_statement( con_ref = lo_con ).
        data(lo_result) = lo_sql->execute_query( statement = lv_sql ).
        lo_result->set_param_table( exporting itab_ref = ref #( et_source ) ).
        lo_result->next_package( ).
      catch cx_sql_exception.    "
      catch cx_parameter_invalid.    "
    endtry.
  endmethod.

  method fetch_sql.
    "-------------------------------------------
    " get list of schemas from connection
    "-------------------------------------------
    try.
        data(lo_con) = cl_sql_connection=>get_connection( exporting con_name = p_dbs ).
        data(lo_sql) = new cl_sql_statement( con_ref = lo_con ).
*        data(lo_result) = lo_sql->execute_query( statement = |select owner, type, OBJECT_NAME, text from all_statements | ).
*        data(lo_result) = lo_sql->execute_query( statement = |select SQL_TEXT from ALL_SQLSET_STATEMENTS | ).
        data(lo_result) = lo_sql->execute_query(
          statement = |SELECT sql_text FROM GV$SQL |
        ).
        lo_result->set_param_table( exporting itab_ref = ref #( et_sql ) ).
        lo_result->next_package( ).
        lo_result->close( ).
        lo_con->close( ).
      catch cx_sql_exception.    "
      catch cx_parameter_invalid.    "
    endtry.
  endmethod.

endclass.                    "lcl_dbview_processor IMPLEMENTATION

*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class lcl_alv_tables_ui implementation.

  method constructor.
    mo_dblink = io_dblink.
  endmethod.

  method show_alv.

    data : lt_fieldcat type slis_t_fieldcat_alv
         , ls_layout type slis_layout_alv
         .

    lcl_gui=>progress( iv_prcnt = 10 iv_msg = 'Получаем список таблиц через DB Link... ' ).
    mo_dblink->fetch_db_tables( importing et_dbsize = go_appl->mt_dbsize ).

    lcl_gui=>progress( iv_prcnt = 70 iv_msg = 'Выводим список таблиц в ALV... ' ).

    try.
        cl_salv_table=>factory( importing r_salv_table = data(lo_table)
                                changing  t_table      = go_appl->mt_dbsize ).
      catch cx_salv_msg.                                "#EC NO_HANDLER
    endtry.

    data : lo_salv_display type ref to cl_salv_display_settings.
    lo_salv_display = lo_table->get_display_settings( ).
    lo_salv_display->set_striped_pattern( cl_salv_display_settings=>true ).
    lo_salv_display->set_list_header( |Tables: { lines( go_appl->mt_dbsize ) }| ).

    data: lr_functions type ref to cl_salv_functions_list.
    lr_functions = lo_table->get_functions( ).
    lr_functions->set_default( abap_true ).

    data: lr_columns type ref to cl_salv_columns.
    lr_columns = lo_table->get_columns( ).
    lr_columns->set_optimize( abap_true ).
    me->build_fieldcat( changing o_columns = lr_columns ).

*    try.
*        data(lv_icon) = conv string( icon_wd_table ). " icon_complete
*        lr_functions->add_function(
*          name     = me->lc_ucomm_ctable
*          icon     = lv_icon
*          text     = 'Open custom table'
*          tooltip  = 'Try to open custom table'
*          position = if_salv_c_function_position=>right_of_salv_functions ).
*      catch cx_salv_wrong_call cx_salv_existing.
*    endtry.

    data lo_events type ref to cl_salv_events.
    lo_events = lo_table->get_event( ).
    set handler me->dbl_click for lo_table->get_event( ).
*    set handler me->on_click_toolbar for lo_events.
    lo_table->display( ).

  endmethod.                    "show_alv
  method build_fieldcat.

    try.
        data(lo_column) = o_columns->get_column( columnname = 'OWNER' ).
        if mo_dblink->is_skip_scheme( ) = abap_true.
          lo_column->set_long_text( value = 'Scheme' ).
        else.
          lo_column->set_visible( if_salv_c_bool_sap=>false ).
        endif.
      catch cx_salv_not_found.    "
    endtry.

    try.
        lo_column = o_columns->get_column( columnname = 'NAME' ).
        lo_column->set_long_text( value = 'Table name' ).
      catch cx_salv_not_found.    "
    endtry.

    try.
        lo_column = o_columns->get_column( columnname = 'ROWS' ).
        lo_column->set_long_text( value = 'Rows' ).
      catch cx_salv_not_found.    "
    endtry.

  endmethod.                    "build_fieldcat

  method dbl_click.
    field-symbols <dbsize> type line of lif_dblink=>tt_dbsize.

    read table go_appl->mt_dbsize assigning field-symbol(<ls_dbsize>)
      index row.
    if <ls_dbsize> is not assigned.
      exit.
    endif.

    case column.
      when 'NAME'.
        if go_alv_columns_ui is not initial.
          go_alv_columns_ui->free( ).
          free go_alv_columns_ui.
        endif.
        go_alv_columns_ui = new #( go_appl->get_dblink( ) ).
        go_alv_columns_ui->set_table_name( <ls_dbsize>-name ).
        go_alv_columns_ui->show_alv( ).
    endcase.

  endmethod.

  method on_click_toolbar.

    case e_salv_function.
      when me->lc_ucomm_ctable.
        message 'test z-table!' type 'I'.
    endcase.

  endmethod.

endclass.                    "lcl_alv_tables_ui DEFINITION

*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class lcl_alv_procs_ui implementation.

  method constructor.
    mo_dblink = io_dblink.
  endmethod.

  method show_alv.

    lcl_gui=>progress( iv_prcnt = 10 iv_msg = 'Получаем список хранимых процедур через DB Link... ' ).
    mo_dblink->fetch_procs( importing et_procs = go_appl->mt_procs ).

    lcl_gui=>progress( iv_prcnt = 70 iv_msg = 'Выводим список хранимых процедур в ALV... ' ).

    try.
        cl_salv_table=>factory( importing r_salv_table = data(lo_table)
                                changing  t_table      = go_appl->mt_procs ).
      catch cx_salv_msg.                                "#EC NO_HANDLER
    endtry.

    data: lr_functions type ref to cl_salv_functions_list.
    lr_functions = lo_table->get_functions( ).
    lr_functions->set_default( abap_true ).

    data: lr_columns type ref to cl_salv_columns.
    lr_columns = lo_table->get_columns( ).
    lr_columns->set_optimize( abap_true ).
    me->build_fieldcat( changing o_columns = lr_columns ).

    set handler me->dbl_click for lo_table->get_event( ).
    lo_table->display( ).

  endmethod.                    "show_alv
  method build_fieldcat.

    try.
        data(lo_column) = o_columns->get_column( columnname = 'OWNER' ).
        lo_column->set_long_text( value = 'Owner' ).
      catch cx_salv_not_found.    "
    endtry.

    try.
        lo_column = o_columns->get_column( columnname = 'OBJECT_NAME' ).
        lo_column->set_long_text( value = 'Object name' ).
      catch cx_salv_not_found.    "
    endtry.

    try.
        lo_column = o_columns->get_column( columnname = 'PROCEDURE_NAME' ).
        lo_column->set_long_text( value = 'Procedure name' ).
      catch cx_salv_not_found.    "
    endtry.

    try.
        lo_column = o_columns->get_column( columnname = 'OBJECT_TYPE' ).
        lo_column->set_long_text( value = 'Type' ).
      catch cx_salv_not_found.    "
    endtry.

  endmethod.                    "build_fieldcat

  method dbl_click.
    field-symbols <dbsize> type line of lif_dblink=>tt_dbsize.

    read table go_appl->mt_procs assigning field-symbol(<ls_procs>)
      index row.
    if <ls_procs> is not assigned.
      exit.
    endif.

    case column.
      when 'OBJECT_NAME' or 'PROCEDURE_NAME'.
        if mo_proc_source_ui is bound.
          mo_proc_source_ui->free( ).
          free mo_proc_source_ui.
        endif.
        mo_proc_source_ui = new #( go_appl->get_dblink( ) ).
        mo_proc_source_ui->show( iv_objname  = <ls_procs>-object_name
                                 iv_procname = <ls_procs>-procedure_name ).
    endcase.

  endmethod.

endclass.                    "lcl_alv_procs_ui DEFINITION

*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class lcl_proc_source_ui implementation.

  method constructor.
    mo_dblink = io_dblink.
  endmethod.
  method show.

    data lo_columns type ref to cl_salv_columns_table.

*   Get data from DB Link
    lcl_gui=>progress( iv_prcnt = 10 iv_msg = 'Получаем список колонок в таблице через DB Link... ' ).
    mo_dblink->fetch_procs_source( exporting iv_objname  = iv_objname
                                             iv_procname = iv_procname
                                   importing et_source   = data(lt_source) ).
    sort lt_source by line.

*   Prepare GUI
    lcl_gui=>progress( iv_prcnt = 70 iv_msg = 'Выводим текст хранимой процедуры... ' ).

    mo_dock = new #( ).
    mo_dock->dock_at( mo_dock->dock_at_right ).
    mo_dock->set_extension( 800 ).

*   find stored proc in source
    data : lt_text type standard table of char255
         .
    lt_text = value #( for line in lt_source
                        ( conv #( line-text ) ) ).
    if iv_procname is not initial.
      find first occurrence of |procedure { iv_procname }| in table lt_text
        in character mode ignoring case
        results data(ls_result).
      if sy-subrc <> 0.
        find first occurrence of |function { iv_procname }| in table lt_text
          in character mode ignoring case
          results ls_result.
      endif.

      if ls_result-line > 40.
        ls_result-line = ls_result-line - 2 .
      endif.
    endif.

    mo_editor = new #( wordwrap_mode     = cl_gui_textedit=>wordwrap_at_fixed_position
                       wordwrap_position = 254
                       wordwrap_to_linebreak_mode = cl_gui_textedit=>true
                       parent            = mo_dock ).
    mo_editor->set_text_as_r3table(
      exporting
        table           = lt_text
      exceptions
        error_dp        = 1
        error_dp_create = 2
        others          = 3
    ).
    mo_editor->set_comments_string( exporting comments_string = '-' ).
    mo_editor->set_highlight_comments_mode( ).


    mo_editor->set_readonly_mode( ).
    if ls_result is not initial.
      mo_editor->go_to_line( exporting  line                   = ls_result-line
                             exceptions error_cntl_call_method = 1
                                        others                 = 2 ).
    endif.

  endmethod.                    "show_alv
  method free.
    free mo_editor.
    mo_dock->free( ).
    free mo_dock.
  endmethod.                    "free
*  method on_click_toolbar.
*
*    case e_salv_function.
*      when me->lc_ucomm_tablecont.
*        if mo_alv_table_data_ui is bound.
*          free mo_alv_table_data_ui.
*        endif.
*        mo_alv_table_data_ui = new #( mo_dblink ).
*        mo_alv_table_data_ui->show_alv( exporting iv_tabname = lv_tabnm
*                                                  it_columns = lt_columns ).
*      when me->lc_ucomm_ztable.
*        message 'test z-table!' type 'I'.
*    endcase.
*
*  endmethod.                    "on_click_toolbar

endclass.                    "lcl_alv_columns_ui IMPLEMENTATION

*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class lcl_salv_columns_ui implementation.

  method constructor.
    mo_dblink = io_dblink.
  endmethod.
  method set_table_name.
    lv_tabnm = iv_tabnm.
  endmethod.                    "set_table_name
  method get_table_name.
    ev_tabnm = lv_tabnm.
  endmethod.                    "get_table_name
  method show_alv.

    data lo_columns type ref to cl_salv_columns_table.

*   Get data from DB Link
    lcl_gui=>progress( iv_prcnt = 10 iv_msg = 'Получаем список колонок в таблице через DB Link... ' ).
    mo_dblink->fetch_table_columns( exporting iv_tabname = lv_tabnm importing et_columns = lt_columns ).

*   Prepare GUI
    lcl_gui=>progress( iv_prcnt = 70 iv_msg = 'Выводим список колонок в ALV... ' ).

    create object lo_dock.
    lo_dock->dock_at( lo_dock->dock_at_right ).
    lo_dock->set_extension( 600 ).
    try.
        cl_salv_table=>factory(
          exporting
            r_container  = lo_dock
          importing
            r_salv_table = lo_salv
          changing
            t_table      = lt_columns ).
      catch cx_salv_msg.                                "#EC NO_HANDLER
    endtry.
    lo_columns = lo_salv->get_columns( ).
    me->build_fieldcat( exporting io_columns = lo_columns ).
    lo_columns->set_optimize( lo_columns->true ).

    data : lo_salv_display type ref to cl_salv_display_settings
         , lo_salv_layout type ref to cl_salv_layout
         , lo_salv_functions type ref to cl_salv_functions
         , st_alv_layoutkey type salv_s_layout_key
         .
    lo_salv_display = lo_salv->get_display_settings( ).
    lo_salv_display->set_striped_pattern( cl_salv_display_settings=>true ).
    lo_salv_display->set_list_header( |Table: { lv_tabnm }| ).
    lo_salv_layout = lo_salv->get_layout( ).
    st_alv_layoutkey-report = sy-repid.
    lo_salv_layout->set_key( st_alv_layoutkey ).
    lo_salv_functions = lo_salv->get_functions( ).
    lo_salv_functions->set_all( abap_true ).

    data lv_icon type string.
    try.
        lv_icon = icon_database_table.
        lo_salv_functions->add_function(
          name     = me->lc_ucomm_tablecont
          icon     = lv_icon
          text     = 'Table content'
          tooltip  = 'Show table content'
          position = if_salv_c_function_position=>right_of_salv_functions ).
      catch cx_salv_wrong_call cx_salv_existing.
    endtry.

    try.
        lv_icon = icon_wd_table. " icon_complete
        lo_salv_functions->add_function(
          name     = me->lc_ucomm_ztable
          icon     = lv_icon
          text     = 'Create Z-table'
          tooltip  = 'Create Z-table with the same structure'
          position = if_salv_c_function_position=>right_of_salv_functions ).
      catch cx_salv_wrong_call cx_salv_existing.
    endtry.

    data lo_events type ref to cl_salv_events.
    lo_events = lo_salv->get_event( ).
    set handler me->on_click_toolbar for lo_events.
    lo_salv->display( ).

  endmethod.                    "show_alv
  method build_fieldcat.

    data: lr_column type ref to cl_salv_column.

    try.
        lr_column = io_columns->get_column( 'COLUMN' ).
        lr_column->set_short_text( 'Column' ).
      catch cx_salv_not_found.                          "#EC NO_HANDLER
    endtry.

    try.
        lr_column = io_columns->get_column( 'TYPE' ).
        lr_column->set_short_text( 'Type' ).
      catch cx_salv_not_found.                          "#EC NO_HANDLER
    endtry.

    try.
        lr_column = io_columns->get_column( 'LENGTH' ).
        lr_column->set_short_text( 'Length' ).
      catch cx_salv_not_found.                          "#EC NO_HANDLER
    endtry.

    try.
        lr_column = io_columns->get_column( 'CONSTRAINT' ).
        lr_column->set_medium_text( 'Constraints' ).
      catch cx_salv_not_found.                          "#EC NO_HANDLER
    endtry.

    exit.
  endmethod.                    "build_fieldcat
  method free.
    lo_salv->close_screen( ).
    free lo_salv.
    lo_dock->free( ).
    free lo_dock.
  endmethod.                    "free
  method on_click_toolbar.

    case e_salv_function.
      when me->lc_ucomm_tablecont.
        if mo_alv_table_data_ui is bound.
          free mo_alv_table_data_ui.
        endif.
        mo_alv_table_data_ui = new #( mo_dblink ).
        mo_alv_table_data_ui->show_alv( exporting iv_tabname = lv_tabnm
                                                  it_columns = lt_columns ).
      when me->lc_ucomm_ztable.
        message 'test z-table!' type 'I'.
    endcase.

  endmethod.                    "on_click_toolbar

endclass.                    "lcl_alv_columns_ui IMPLEMENTATION

*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class lcl_salv_table_data_ui implementation.
  method constructor.
    mo_dblink = io_dblink.
  endmethod.
  method show_alv.

    if iv_tabname is supplied.
      lv_tabname = iv_tabname.
    endif.
    if it_columns is supplied.
      lt_columns = it_columns.
    endif.

    field-symbols <tabcont> type table.
    data lo_salv type ref to cl_salv_table.
    data lo_columns type ref to cl_salv_columns_table.
    data lo_functions type ref to cl_salv_functions.
    data lo_display_settings type ref to cl_salv_display_settings.
    data lx_salv_msg type ref to cx_salv_msg.

    data lv_lines type int4.
    data lv_title type lvc_title.

    lcl_gui=>progress( iv_prcnt = 10 iv_msg = 'Получаем содержимое таблицы через DB Link... ' ).
    mo_dblink->fetch_table_data( exporting iv_tabnm   = lv_tabname
                                           it_columns = lt_columns
                                 importing ed_data    = ld_tabcont ).
    lcl_gui=>progress( iv_prcnt = 70 iv_msg = 'Выводим содержимое таблицы в ALV... ' ).
    assign ld_tabcont->* to <tabcont>.
    if <tabcont> is assigned.
      try.
          cl_salv_table=>factory( importing r_salv_table = lo_salv
                                  changing  t_table = <tabcont> ).
        catch cx_salv_msg into lx_salv_msg.
          message lx_salv_msg type 'E'.
      endtry.
      describe table <tabcont> lines lv_lines.
      write lv_lines to lv_title.
      condense lv_title.
      concatenate 'Number of records:' lv_title into lv_title separated by space.
      lo_columns = lo_salv->get_columns( ).
      me->build_fieldcat( lo_columns ).
      lo_columns->set_optimize( abap_true ).
      lo_functions = lo_salv->get_functions( ).
      lo_functions->set_all( abap_true ).
      lo_display_settings = lo_salv->get_display_settings( ).
      lo_display_settings->set_list_header( lv_title ).
      lo_salv->display( ).
    else.
      message 'Table with data is empty!' type 'W'.
    endif.

  endmethod.                    "show_alv
  method build_fieldcat.
    data lt_column_ref type salv_t_column_ref.
    data lv_text type scrtext_m.
    field-symbols <column_ref> type line of salv_t_column_ref.

    lt_column_ref = io_columns->get( ).
    loop at lt_column_ref assigning <column_ref>.
      lv_text = <column_ref>-columnname.
      <column_ref>-r_column->set_medium_text( lv_text ).
    endloop.

  endmethod.                    "build_fieldcat
endclass.                    "lcl_salv_table_data_ui IMPLEMENTATION
