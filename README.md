# ABAP - Dynamic selection screen
A fully dynamic selection screen with FREE_SELECTION function modules in SAP ABAP 7.50

![image](https://github.com/user-attachments/assets/81fd338d-07ae-405f-a659-2f174de0896e)


> [!WARNING]
> Work in progress.

The situation: We wanted a report with a selection screen for a large application that would allow users to restrict data, given that the application handles a lot of it. In the old version of the report, we created the selection screen manually, adding boxes and icons to indicate status. However, as the application grew, our customer wanted to include Z-fields in the database and select data by Z-field. Some of these fields were transferred to our standard application, but some customers did not need them. Therefore, we manually added these fields to the selection screen and removed them at runtime if they were not needed.

With the new implementation of the report, we would like to have a dynamic selection screen that can grow with our software and customer wishes. SAP already has such an option: FREE_SELECTION_DIALOG.

However, if you use the FREE_SEL* function modules, you must accept certain limitations. Adding this to a selection screen is easy — just Google it — but adding your own touch, such as icons for the status, will cause issues. The FREE_SELECTION_DIALOG, on the other hand, has an AS_SUBSCREEN parameter. This sub-screen can be added to the selection screen along with other own sub-selectino-screens.

In this example, we will use the FREE_SEL* function modules to select EKKO (purchase order header data) and add an additional field for the maximum number of rows selected — our own touch, in this case.

First, create a basic selection screen with some dummy ```TABBED BLOCK``` to allow us to use if for our sub-screens. 
```abap
" Create dummy tabbed section to get the sub screens. 
SELECTION-SCREEN BEGIN OF TABBED BLOCK sub_dny_sel FOR 10 LINES.
SELECTION-SCREEN END OF BLOCK sub_dny_sel.

SELECTION-SCREEN BEGIN OF TABBED BLOCK sub_own_sel FOR 3 LINES.
SELECTION-SCREEN END OF BLOCK sub_own_sel.

" Create section for additional selection data in order to allow more
" specific selection like UP TO n ROWS. 
SELECTION-SCREEN BEGIN OF SCREEN 200 AS SUBSCREEN.
  SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE title.
    PARAMETERS pa_max TYPE numc5.
  SELECTION-SCREEN END OF BLOCK b2.
SELECTION-SCREEN END OF SCREEN 200.
```

During the report's initialization event, we set up the initial selection fields and configured the FREE_SEL* function modules with INIT and DIALOG.

```abap
INITIALIZATION.
  title = 'Own additional selection'.

  " Set the first sub dynpro to 1105 of the 'FREE_SELECTION*' function group.
  " You can also use 1106 if you want the short version of ranges or
  " use 2000 if you want to use the tree.
  sub_dny_sel-prog  = 'SAPLSSEL'.
  sub_dny_sel-dynnr = '1105'.

  " Set second sub dynpro to our own screen number. You can also add more sub screens if needed.
  sub_own_sel-prog  = sy-repid.
  sub_own_sel-dynnr = 200.

  " Get and set the PF status from report RSSYSTDB for a clear UI.
  " You can also try the other one of that report.
  DATA(pf_status) = VALUE rsdspfkey( pfkey   = '%_FS_P_O'
                                     program = 'RSSYSTDB' ).
  SET PF-STATUS pf_status.

  DATA selection_id TYPE rsdynsel-selid.
  DATA condition    TYPE rsds_twhere.

  " Set you data for the free selection. You can also join more than one table here if needed. 
  DATA(selection_table) = VALUE rsdstabs_t( ( prim_tab = 'EKKO' ) ).
  DATA(selection_fields) = VALUE rsdsfields_t( tablename = 'EKKO'
                                               ( fieldname = 'EBELN' )
                                               ( fieldname = 'BSART' ) ).

  " Init the free selection function and receive the id. 
  CALL FUNCTION 'FREE_SELECTIONS_INIT'
    EXPORTING  kind         = 'T'
    IMPORTING  selection_id = selection_id
    TABLES     tables_tab   = selection_table
               fields_tab   = selection_fields
    EXCEPTIONS OTHERS       = 20.

  IF sy-subrc <> 0.
    RETURN.
  ENDIF.

  " Trigger the dialog with the AS_SUBSCREEN = true.
  CALL FUNCTION 'FREE_SELECTIONS_DIALOG'
    EXPORTING  selection_id    = selection_id
               pfkey           = pf_status
               tree_visible    = abap_false
               no_frame        = abap_false
               as_subscreen    = abap_true
    IMPORTING  where_clauses   = condition
    TABLES     fields_tab      = selection_fields
    EXCEPTIONS internal_error  = 1
               no_action       = 2
               selid_not_found = 3
               illegal_status  = 4
               OTHERS          = 5.
```
During the START-OF-SELECTION event, we receive the condition and build a fully dynamic select. Finally, we show the data to the user.

```abap
START-OF-SELECTION.
" For this example, we only need the where clauses. So we get it by
" performing an external on GEN_WHERE_CLAUSES in report SAPLSSEL.
" Before we need to get the information about current selection id.
  ASSIGN ('(SAPLSSEL)CURRENT_INFO') TO FIELD-SYMBOL(<sel_info>).
  PERFORM gen_where_clauses(saplssel) USING    <sel_info>
                                      CHANGING condition
                                               sy-subrc.

  " Create fully dynamic select according to free selection condition.
  TRY.
      DATA(table_name) = selection_table[ 1 ]-prim_tab.
    CATCH cx_root.
      RETURN.
  ENDTRY.

  DATA data_ref TYPE REF TO data.
  FIELD-SYMBOLS <selected_data> TYPE INDEX TABLE.

  CREATE DATA data_ref TYPE STANDARD TABLE OF (table_name).
  ASSIGN data_ref->* TO <selected_data>.

  DATA(where_clause) = condition[ tablename = table_name ].

  SELECT * FROM (table_name) INTO TABLE <selected_data> UP TO pa_max ROWS WHERE (where_clause-where_tab).

  " Output the selected data to user
  cl_demo_output=>display( <selected_data> ).
```
