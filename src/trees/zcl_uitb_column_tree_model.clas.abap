"! <p class="shorttext synchronized" lang="en">Column Tree Model</p>
CLASS zcl_uitb_column_tree_model DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_uitb_content_searcher .
    INTERFACES zif_uitb_gui_control .

    CONSTANTS c_single_selection TYPE i VALUE cl_tree_model=>node_sel_mode_single ##NO_TEXT.
    CONSTANTS c_multiple_selection TYPE i VALUE cl_tree_model=>node_sel_mode_multiple ##NO_TEXT.
    CONSTANTS c_dnd_move TYPE i VALUE cl_dragdrop=>move ##NO_TEXT.
    CONSTANTS c_dnd_copy TYPE i VALUE cl_dragdrop=>copy ##NO_TEXT.
    CONSTANTS c_dnd_none TYPE i VALUE cl_dragdrop=>none ##NO_TEXT.
    CONSTANTS c_hierarchy_column TYPE tv_itmname VALUE 'HIERARCHY' ##NO_TEXT.

    ALIASES has_focus
      for zif_uitb_gui_control~has_focus.

    METHODS constructor
      IMPORTING
        !ir_parent           TYPE REF TO cl_gui_container
        !is_hierarchy_header TYPE treemhhdr
        !if_item_selection   TYPE abap_bool OPTIONAL
        !if_with_toolbar     TYPE abap_bool OPTIONAL
        if_auto_node_key     TYPE abap_bool OPTIONAL
        !iv_selection_mode   TYPE i DEFAULT c_single_selection .
    "! <p class="shorttext synchronized" lang="en">Scroll to position in tree</p>
    METHODS scroll_to
      IMPORTING
        iv_position TYPE i DEFAULT cl_tree_model=>scroll_home.
    METHODS create_tree_control .
    "! <p class="shorttext synchronized" lang="en">Get toolbar of tree (only bound for if_with_toolbar = 'X')</p>
    METHODS get_toolbar
      RETURNING
        VALUE(result) TYPE REF TO zcl_uitb_toolbar_model .
    METHODS update_view
      RAISING
        zcx_uitb_tree_error .
    "! <p class="shorttext synchronized" lang="en">Get nodes object for tree</p>
    METHODS get_nodes
      RETURNING
        VALUE(result) TYPE REF TO zcl_uitb_ctm_nodes .
    "! <p class="shorttext synchronized" lang="en">Get selections object for tree</p>
    METHODS get_selections
      RETURNING
        VALUE(result) TYPE REF TO zcl_uitb_ctm_selections .
    "! <p class="shorttext synchronized" lang="en">Get events object for tree</p>
    METHODS get_events
      RETURNING
        VALUE(result) TYPE REF TO zcl_uitb_ctm_events .
    "! <p class="shorttext synchronized" lang="en">Get columns object for tree</p>
    METHODS get_columns
      RETURNING
        VALUE(result) TYPE REF TO zcl_uitb_ctm_columns .
    "! <p class="shorttext synchronized" lang="en">Get Search object for tree</p>
    METHODS get_search
      RETURNING
        VALUE(result) TYPE REF TO zcl_uitb_ctm_search .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mr_dnd_behavior TYPE REF TO cl_dragdrop .
    DATA mv_selection_mode TYPE i .
    DATA ms_hierarchy_header TYPE treemhhdr .
    DATA mr_tree_model TYPE REF TO cl_column_tree_model .
    DATA mr_tree_container TYPE REF TO zcl_uitb_tree_container .
    data mo_tree_ctrl type ref to cl_gui_control.
    DATA mr_parent TYPE REF TO cl_gui_container .
    DATA mr_toolbar_model TYPE REF TO zcl_uitb_toolbar_model .
    DATA mf_with_toolbar TYPE abap_bool .
    DATA mf_item_selection TYPE abap_bool .
    DATA mr_selections TYPE REF TO zcl_uitb_ctm_selections .
    DATA mr_columns TYPE REF TO zcl_uitb_ctm_columns .
    DATA mr_nodes TYPE REF TO zcl_uitb_ctm_nodes .
    "! <p class="shorttext synchronized" lang="en">Search functionality for Column Tree Model</p>
    DATA mr_search TYPE REF TO zcl_uitb_ctm_search .
    DATA mr_events TYPE REF TO zcl_uitb_ctm_events .
    DATA mr_temp_control TYPE REF TO cl_gui_control .
    DATA mf_auto_node_key TYPE abap_bool.

    METHODS create_tree_model.
ENDCLASS.



CLASS zcl_uitb_column_tree_model IMPLEMENTATION.


  METHOD constructor.
    mr_parent = ir_parent.
    mf_item_selection = if_item_selection.
    mv_selection_mode = iv_selection_mode.
    ms_hierarchy_header = is_hierarchy_header.
    mf_with_toolbar = if_with_toolbar.
    mf_auto_node_key = if_auto_node_key.

    create_tree_model(  ).
  ENDMETHOD.


  METHOD create_tree_control.
    mr_tree_container = zcl_uitb_tree_container=>create_container(
        if_create_toolbar = mf_with_toolbar
        ir_parent         = mr_parent
        ir_tree_model     = mr_tree_model
    ).
    " create tree control from model
    mo_tree_ctrl = mr_tree_container->get_tree( ).

    IF mf_with_toolbar = abap_true.
      DATA(lr_toolbar) = mr_tree_container->get_toolbar( ).
      mr_toolbar_model->connect_control( lr_toolbar ).
      mr_toolbar_model->refresh_ui( ).
    ENDIF.
  ENDMETHOD.


  METHOD create_tree_model.
    mr_tree_model = NEW #(
        node_selection_mode   = mv_selection_mode
        item_selection        = mf_item_selection
        hierarchy_header      = ms_hierarchy_header
        hierarchy_column_name = c_hierarchy_column
    ).

    IF mf_with_toolbar = abap_true.
      mr_toolbar_model = NEW #( ).
    ENDIF.

    get_events( ).
    get_selections( ).
    get_nodes( ).
    get_columns( ).
  ENDMETHOD.


  METHOD get_columns.
    IF mr_columns IS INITIAL.
      mr_columns = NEW #(
        ir_model = mr_tree_model
      ).
    ENDIF.

    result = mr_columns.
  ENDMETHOD.


  METHOD get_events.
    IF mr_events IS INITIAL.
      mr_events = NEW #(
          if_item_selection = mf_item_selection
          if_single_selection = get_selections( )->is_single_selection( )
          ir_model            = mr_tree_model
      ).
    ENDIF.

    result = mr_events.
  ENDMETHOD.


  METHOD get_nodes.
    IF mr_nodes IS INITIAL.
      mr_nodes = NEW #(
          ir_model         = mr_tree_model
          if_auto_node_key = mf_auto_node_key
      ).
    ENDIF.

    result = mr_nodes.
  ENDMETHOD.


  METHOD get_search.
    IF mr_search IS INITIAL.
      mr_search = NEW #( mr_tree_model ).
    ENDIF.

    result = mr_search.
  ENDMETHOD.


  METHOD get_selections.
    IF mr_selections IS INITIAL.
      mr_selections = NEW #(
          ir_model          = mr_tree_model
          ir_nodes          = get_nodes( )
          if_item_selection = mf_item_selection
          iv_selection_mode = mv_selection_mode
      ).
    ENDIF.

    result = mr_selections.
  ENDMETHOD.


  METHOD get_toolbar.
    result = mr_toolbar_model.
  ENDMETHOD.


  METHOD update_view.
    mr_tree_model->update_view(
      EXCEPTIONS
        control_not_existing = 1
        control_dead         = 2
        cntl_system_error    = 3
        failed               = 4
        OTHERS               = 5
    ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_uitb_tree_error.
    ENDIF.
  ENDMETHOD.


  METHOD zif_uitb_content_searcher~search.
    mr_tree_model->find(
        IMPORTING result_item_key_table = DATA(lt_result_item)
                  result_type           = DATA(lv_result_type)
    ).

    IF lv_result_type <> 0 AND lt_result_item IS NOT INITIAL.
      get_selections( )->select_nodes( VALUE #( ( lt_result_item[ 1 ]-node_key ) ) ).
    ENDIF.
  ENDMETHOD.


  METHOD zif_uitb_content_searcher~search_next.
    mr_tree_model->find_next(
        IMPORTING result_item_key_table    = DATA(lt_result_item)
                  result_expander_node_key = DATA(lt_result_expander_node_key)
                  result_type              = DATA(lv_result_type)
    ).

    IF lv_result_type <> 0 AND lt_result_item IS NOT INITIAL.
      get_selections( )->select_nodes( VALUE #( ( lt_result_item[ 1 ]-node_key ) ) ).
    ENDIF.
  ENDMETHOD.


  METHOD zif_uitb_gui_control~focus.
    zcl_uitb_gui_helper=>set_focus( mo_tree_ctrl ).
  ENDMETHOD.


  METHOD zif_uitb_gui_control~has_focus.
    rf_has_focus = zcl_uitb_gui_helper=>has_focus( mo_tree_ctrl ).
  ENDMETHOD.

  METHOD scroll_to.
    mr_tree_model->scroll(
      EXPORTING
        scroll_command         = iv_position
      EXCEPTIONS
        control_not_existing   = 1
        control_dead           = 2
        cntl_system_error      = 3
        failed                 = 4
        illegal_scroll_command = 5
        OTHERS                 = 6
    ).
    IF sy-subrc <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*       WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
