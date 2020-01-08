class ZCL_ALV_EVENT_HANDLER definition
  public
  final
  create public .

public section.

  data M_REPID type PROGNAME .
  data M_F4_FORM type STRING .
  data M_TOOLBAR_FORM type STRING .
  data M_HOTSPOT_FORM type STRING .
  data M_USER_COMMAND_FORM type STRING .
  data M_DATACHANGED_FORM type STRING .
  data M_DATACHANGED_FINISHED_FORM type STRING .
  data M_BEFORE_UCOMM_FORM type STRING .
  data M_DOUBLE_CLICK_FORM type STRING .
  data M_MENU_BUTTON_FORM type STRING .

  methods CONSTRUCTOR
    importing
      !IO_ALV type ref to CL_GUI_ALV_GRID
      !I_REPID type PROGNAME default SY-CPROG
      !I_F4_FORM type STRING optional
      !I_TOOLBAR_FORM type STRING optional
      !I_USER_COMMAND_FORM type STRING optional
      !I_HOTSPOT_FORM type STRING optional
      !I_DATACHANGED_FORM type STRING optional
      !I_DATACHANGED_FINISHED_FORM type STRING optional
      !I_BEFORE_UCOMM_FORM type STRING optional
      !I_DOUBLE_CLICK_FORM type STRING optional
      !I_MENU_BUTTON_FORM type STRING optional .
  methods HANDLE_F4
    for event ONF4 of CL_GUI_ALV_GRID
    importing
      !E_FIELDNAME
      !E_FIELDVALUE
      !ES_ROW_NO
      !ER_EVENT_DATA
      !ET_BAD_CELLS
      !E_DISPLAY .
  methods HANDLE_HOTSPOT
    for event HOTSPOT_CLICK of CL_GUI_ALV_GRID
    importing
      !E_ROW_ID
      !E_COLUMN_ID
      !ES_ROW_NO .
  methods HANDLE_TOOLBAR
    for event TOOLBAR of CL_GUI_ALV_GRID
    importing
      !E_OBJECT
      !E_INTERACTIVE .
  methods HANDLE_USER_COMMAND
    for event USER_COMMAND of CL_GUI_ALV_GRID
    importing
      !E_UCOMM .
  methods HANDLE_DATACHANGED
    for event DATA_CHANGED of CL_GUI_ALV_GRID
    importing
      !ER_DATA_CHANGED
      !E_ONF4
      !E_ONF4_BEFORE
      !E_ONF4_AFTER
      !E_UCOMM .
  methods HANDLE_DATACHANGED_FINISHED
    for event DATA_CHANGED_FINISHED of CL_GUI_ALV_GRID
    importing
      !E_MODIFIED
      !ET_GOOD_CELLS .
  methods HANDLE_BEFORE_UCOMM
    for event BEFORE_USER_COMMAND of CL_GUI_ALV_GRID
    importing
      !E_UCOMM .
  methods HANDLE_DOUBLE_CLICK
    for event DOUBLE_CLICK of CL_GUI_ALV_GRID
    importing
      !E_ROW
      !E_COLUMN
      !ES_ROW_NO .
  methods HANDLE_MENU_BUTTON
    for event MENU_BUTTON of CL_GUI_ALV_GRID
    importing
      !E_OBJECT
      !E_UCOMM .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ALV_EVENT_HANDLER IMPLEMENTATION.


METHOD constructor.
  DEFINE d_set_handler.
    IF &1 IS SUPPLIED.
      &2 = &1.
      SET HANDLER me->&3 FOR io_alv.
    ENDIF.
  END-OF-DEFINITION.

  m_repid = i_repid.

  d_set_handler:
*   构造函数入参                  类属性                        类方法
    i_f4_form                     m_f4_form                     handle_f4,
    i_toolbar_form                m_toolbar_form                handle_toolbar,
    i_user_command_form           m_user_command_form           handle_user_command,
    i_hotspot_form                m_hotspot_form                handle_hotspot,
    i_datachanged_form            m_datachanged_form            handle_datachanged,
    i_datachanged_finished_form   m_datachanged_finished_form   handle_datachanged_finished,
    i_before_ucomm_form           m_before_ucomm_form           handle_before_ucomm,
    i_double_click_form           m_double_click_form           handle_double_click,
    i_menu_button_form            m_menu_button_form            handle_menu_button.
ENDMETHOD.                    "constructor


METHOD handle_before_ucomm.
  PERFORM (m_before_ucomm_form) IN PROGRAM (m_repid) IF FOUND
    USING e_ucomm.
*FORM 示例
*FORM handle_before_ucomm USING e_ucomm TYPE sy-ucomm.
*ENDFORM.                    "handle_before_ucomm
ENDMETHOD.                    "handle_before_ucomm


METHOD handle_datachanged.
  PERFORM (m_datachanged_form) IN PROGRAM (m_repid) IF FOUND
    USING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm.
*FORM 示例
*FORM handle_datachanged_cpmx USING  er_data_changed TYPE REF TO cl_alv_changed_data_protocol
*                                    e_onf4 TYPE char01
*                                    e_onf4_before TYPE char01
*                                    e_onf4_after TYPE char01
*                                    e_ucomm TYPE sy-ucomm.
*  FIELD-SYMBOLS: <ls_cell> TYPE lvc_s_modi.
*  DATA: ls_cpmx TYPE ty_cpmx,
*        lt_mod_cells TYPE lvc_t_modi WITH HEADER LINE.
*
*  CLEAR: g_error.
*
*  DEFINE add_protocol_entry.
*    g_error = 'X'.
*    call method er_data_changed->add_protocol_entry
*      exporting
*        i_msgid     = &1
*        i_msgty     = &2
*        i_msgno     = &3
*        i_msgv1     = &4
*        i_msgv2     = &5
*        i_msgv3     = &6
*        i_msgv4     = &7
*        i_fieldname = <ls_cell>-fieldname
*        i_row_id    = <ls_cell>-row_id
*        i_tabix     = <ls_cell>-tabix.
*  END-OF-DEFINITION.
*  DEFINE modify_cell.
*    call method er_data_changed->modify_cell
*      exporting
*        i_row_id    = <ls_cell>-row_id
*        i_tabix     = <ls_cell>-tabix
*        i_fieldname = &1
*        i_value     = &2.
*  END-OF-DEFINITION.
*
*  LOOP AT er_data_changed->mt_mod_cells ASSIGNING <ls_cell>.
*    CASE <ls_cell>-fieldname.
*      WHEN 'BUKRS'.  "公司代码描述
*        CLEAR gs_t001.
*        READ TABLE gt_t001 INTO gs_001 WITH KEY bukrs = <ls_cell>-value.
*        IF sy-subrc <> 0 AND <ls_cell>-value IS NOT INITIAL.
*          add_protocol_entry: '00' 'E' '001' '公司代码:' <ls_cell>-value '不存在' ''.
*        ENDIF.
*        modify_cell: 'BUTXT' gs_001-butxt.
*    ENDCASE.
*  ENDLOOP.
ENDMETHOD.                    "handle_datachanged


METHOD handle_datachanged_finished.
  PERFORM (m_datachanged_finished_form) IN PROGRAM (m_repid) IF FOUND
    USING e_modified et_good_cells.
*FORM 示例
*FORM handle_datachanged_finished USING  e_modified TYPE char01
*                                        et_good_cells TYPE lvc_t_modi.
*ENDFORM.                    "HANDLE_DATACHANGED_FINISHED
ENDMETHOD.                    "handle_datachanged_finished


METHOD handle_double_click.
  PERFORM (m_double_click_form) IN PROGRAM (m_repid) IF FOUND
    USING e_row e_column es_row_no.
*FORM 示例
*FORM handle_double_click USING  e_row TYPE lvc_s_row
*                                e_column TYPE lvc_s_col
*                                es_row_no TYPE lvc_s_roid.
*ENDFORM.                    "handle_before_ucomm
ENDMETHOD.                    "HANDLE_DOUBLE_CLICK


METHOD handle_f4.
  PERFORM (m_f4_form) IN PROGRAM (m_repid) IF FOUND
    USING e_fieldname e_fieldvalue es_row_no er_event_data et_bad_cells e_display.
*FORM 示例
*FORM handle_f4  USING           e_fieldname TYPE lvc_fname
*                                e_fieldvalue TYPE lvc_value
*                                es_row_no TYPE lvc_s_roid
*                                er_event_data TYPE REF TO cl_alv_event_data
*                                et_bad_cells TYPE lvc_t_modi
*                                e_display TYPE char01.
*  DATA: ls_row TYPE lvc_s_row,
*        ls_col TYPE lvc_s_col,
*        ls_modi TYPE lvc_s_modi,
*        l_tabname TYPE tabname,
*        l_fieldtext TYPE fieldtext,
*        l_ref_table TYPE lvc_rtname,
*        l_ref_field TYPE lvc_rfname.
*  FIELD-SYMBOLS: <lt_modi> TYPE lvc_t_modi.
*
*  er_event_data->m_event_handled = 'X'.
*
*  CASE e_fieldname.
*    WHEN 'TABNAME'.
*      PERFORM f4_dd_table(rsaqddic) USING 'SAPLAQJD_CNTRL'
*                                          '0300'
*                                          'G_DYN_0300-TNAME'
*                                    CHANGING e_fieldvalue.  "搜索帮助代码，来于SQVI中“插入表”的搜索帮助
*    WHEN OTHERS.
*      EXIT.
*  ENDCASE.
*
*  ASSIGN er_event_data->m_data->* TO <lt_modi>.
*  ls_modi-row_id    = es_row_no-row_id."
*  ls_modi-fieldname = e_fieldname.
*  ls_modi-value     = e_fieldvalue.
*  APPEND ls_modi TO <lt_modi>.
*  IF e_fieldname = 'FIELDNAME'.
*    ls_modi-row_id    = es_row_no-row_id."
*    ls_modi-fieldname = 'SCRTEXT_L'.
*    ls_modi-value     = l_fieldtext.
*    APPEND ls_modi TO <lt_modi>.
*
*    ls_modi-row_id    = es_row_no-row_id."
*    ls_modi-fieldname = 'REF_TABLE'.
*    ls_modi-value     = l_ref_table.
*    APPEND ls_modi TO <lt_modi>.
*
*    ls_modi-row_id    = es_row_no-row_id."
*    ls_modi-fieldname = 'REF_FIELD'.
*    ls_modi-value     = l_ref_field.
*    APPEND ls_modi TO <lt_modi>.
*  ENDIF.
*
*ENDFORM.
ENDMETHOD.                                                "handle_f4


METHOD handle_hotspot.
  PERFORM (m_hotspot_form) IN PROGRAM (m_repid) IF FOUND
    USING e_row_id e_column_id es_row_no.
*FORM 示例
*FORM handle_hotspot  USING    p_row_id     TYPE lvc_s_row
*                              p_column_id  TYPE lvc_s_col
*                              p_row_no     TYPE lvc_s_roid.
*  CASE p_column_id-fieldname.
*    WHEN 'COUNT'.
*  ENDCASE.
*ENDFORM.
ENDMETHOD.                    "handle_hotspot


METHOD handle_menu_button.
  PERFORM (m_menu_button_form) IN PROGRAM (m_repid) IF FOUND USING e_object e_ucomm.
*FORM 示例
*FORM handle_menu_button  USING  e_object TYPE REF TO cl_ctmenu
*                                e_ucomm TYPE sy-ucomm.
*  CASE e_ucomm.
*    WHEN 'MENU'.
*      CALL METHOD e_object->add_function
*        EXPORTING
*          fcode = 'MENU1'
*          text  = '菜单1'.
*  ENDCASE.
*ENDFORM.                    "handle_menu_button
ENDMETHOD.                    "handle_menu_button


METHOD handle_toolbar.
  PERFORM (m_toolbar_form) IN PROGRAM (m_repid) IF FOUND USING e_object e_interactive.
*FORM 示例
*FORM handle_toolbar USING   e_object TYPE REF TO cl_alv_event_toolbar_set
*                            e_interactive TYPE char1.
*  DATA: ls_toolbar TYPE stb_button.
*
*  ls_toolbar-function = 'IMPORT'.
*  ls_toolbar-icon = icon_import.
*  ls_toolbar-text = '导入表格字段'.
*  ls_toolbar-quickinfo = '导入表格字段'.
*  APPEND ls_toolbar TO e_object->mt_toolbar.
*  CLEAR: ls_toolbar.
*
*  ls_toolbar-function = 'ALL'.
*  ls_toolbar-text = '输出：全选'.
*  ls_toolbar-quickinfo = '全部输出'.
*  APPEND ls_toolbar TO e_object->mt_toolbar.
*  CLEAR: ls_toolbar.
*
*  ls_toolbar-function = 'NONE'.
*  ls_toolbar-text = '输出：取消全选'.
*  ls_toolbar-quickinfo = '全部不输出'.
*  APPEND ls_toolbar TO e_object->mt_toolbar.
*  CLEAR: ls_toolbar.
*ENDFORM.
ENDMETHOD.                    "handle_toolbar


METHOD handle_user_command.
  PERFORM (m_user_command_form) IN PROGRAM (m_repid) IF FOUND USING e_ucomm.
*FORM 示例
*FORM handle_user_command  USING    e_ucomm TYPE sy-ucomm.
*  DATA: ok_code TYPE sy-ucomm.
*  ok_code = e_ucomm.
*  CLEAR e_ucomm.
*
*  CASE ok_code.
*    WHEN 'IMPORT'.
*      go_alv->refresh_table_display( ).
*  ENDCASE.
*ENDFORM.
ENDMETHOD.                    "handle_user_command
ENDCLASS.
