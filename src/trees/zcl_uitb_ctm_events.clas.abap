CLASS zcl_uitb_ctm_events DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_uitb_tree_model_events .

    ALIASES button_click
      FOR zif_uitb_tree_model_events~button_click .
    ALIASES checkbox_change
      FOR zif_uitb_tree_model_events~checkbox_change .
    ALIASES drag
      FOR zif_uitb_tree_model_events~drag .
    ALIASES drag_multiple
      FOR zif_uitb_tree_model_events~drag_multiple .
    ALIASES drop
      FOR zif_uitb_tree_model_events~drop .
    ALIASES drop_complete
      FOR zif_uitb_tree_model_events~drop_complete .
    ALIASES drop_complete_multiple
      FOR zif_uitb_tree_model_events~drop_complete_multiple .
    ALIASES expand_no_children
      FOR zif_uitb_tree_model_events~expand_no_children .
    ALIASES item_double_click
      FOR zif_uitb_tree_model_events~item_double_click .
    ALIASES item_keypress
      FOR zif_uitb_tree_model_events~item_keypress .
    ALIASES link_click
      FOR zif_uitb_tree_model_events~link_click .
    ALIASES node_context_menu_request
      FOR zif_uitb_tree_model_events~node_context_menu_request .
    ALIASES node_context_menu_select
      FOR zif_uitb_tree_model_events~node_context_menu_select .
    ALIASES node_double_click
      FOR zif_uitb_tree_model_events~node_double_click .
    ALIASES node_keypress
      FOR zif_uitb_tree_model_events~node_keypress .
    ALIASES selection_changed
      FOR zif_uitb_tree_model_events~selection_changed .

    METHODS constructor
      IMPORTING
        !ir_model            TYPE REF TO cl_column_tree_model
        !if_item_selection   TYPE abap_bool
        !if_single_selection TYPE abap_bool .
    METHODS add_key_for_keypress
      IMPORTING
        !iv_key TYPE i
      RAISING
        zcx_uitb_tree_error .
    METHODS remove_all_key_strokes .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mr_model TYPE REF TO cl_column_tree_model .

    METHODS build_events
      RAISING
        zcx_uitb_tree_error .
    METHODS on_node_double_click
        FOR EVENT node_double_click OF cl_column_tree_model
      IMPORTING
        !node_key .
    METHODS on_tree_drag
        FOR EVENT drag OF cl_column_tree_model
      IMPORTING
        !drag_drop_object
        !item_name
        !node_key .
    METHODS on_tree_drag_multiple
        FOR EVENT drag_multiple OF cl_column_tree_model
      IMPORTING
        !drag_drop_object
        !item_name
        !node_key_table .
    METHODS on_drop_complete
        FOR EVENT drop_complete OF cl_column_tree_model
      IMPORTING
        !drag_drop_object
        !item_name
        !node_key .
    METHODS on_tree_drop_complete_multiple
        FOR EVENT drop_complete_multiple OF cl_column_tree_model
      IMPORTING
        !drag_drop_object
        !item_name
        !node_key_table .
    METHODS on_tree_drop
        FOR EVENT drop OF cl_column_tree_model
      IMPORTING
        !drag_drop_object
        !node_key
        !sender .
    METHODS on_node_context_menu_request
        FOR EVENT node_context_menu_request OF cl_column_tree_model
      IMPORTING
        !menu
        !node_key .
    METHODS on_node_context_menu_select
        FOR EVENT node_context_menu_select OF cl_column_tree_model
      IMPORTING
        !fcode
        !node_key .
    METHODS on_node_key_press
        FOR EVENT node_keypress OF cl_column_tree_model
      IMPORTING
        !key
        !node_key .
    METHODS on_item_key_press
        FOR EVENT item_keypress OF cl_column_tree_model
      IMPORTING
        !item_name
        !key
        !node_key .
    METHODS on_item_double_click
        FOR EVENT item_double_click OF cl_column_tree_model
      IMPORTING
        !item_name
        !node_key .
    METHODS on_expand_no_children
        FOR EVENT expand_no_children OF cl_column_tree_model
      IMPORTING
        !node_key .
    METHODS on_selection_changed
        FOR EVENT selection_changed OF cl_column_tree_model
      IMPORTING
        !node_key .
    METHODS on_item_button_click
        FOR EVENT button_click OF cl_column_tree_model
      IMPORTING
        !item_name
        !node_key .
    METHODS on_item_link_click
        FOR EVENT link_click OF cl_column_tree_model
      IMPORTING
        !item_name
        !node_key .
    METHODS on_checkbox_change
        FOR EVENT checkbox_change OF cl_item_tree_model
      IMPORTING
        !checked
        !item_name
        !node_key .
ENDCLASS.



CLASS zcl_uitb_ctm_events IMPLEMENTATION.


  METHOD add_key_for_keypress.
    mr_model->add_key_stroke(
      EXPORTING
        key         = iv_key
      EXCEPTIONS
        illegal_key = 1
        OTHERS      = 2
    ).
    IF sy-subrc <> 0.
      zcx_uitb_tree_error=>raise_from_sy( ).
    ENDIF.
  ENDMETHOD.


  METHOD build_events.
  ENDMETHOD.


  METHOD constructor.
    DATA: lt_events TYPE cntl_simple_events.

*.. Selection change event only works if the following criteria are met
*.. - no item selection
*.. - no double click event registered
*.. - no multiple selection activated
    mr_model = ir_model.

    lt_events = VALUE #(
        ( eventid = cl_column_tree_model=>eventid_node_context_menu_req  appl_event = abap_true )
        ( eventid = cl_column_tree_model=>eventid_node_double_click      appl_event = abap_true )
        ( eventid = cl_column_tree_model=>eventid_node_keypress          appl_event = abap_true )
     ).

*... add default key stroke ENTER
    add_key_for_keypress( cl_tree_model=>key_enter ).

    IF if_item_selection = abap_true.
      lt_events = VALUE #(
          BASE lt_events
          ( eventid = cl_column_tree_model=>eventid_link_click        appl_event = abap_true )
          ( eventid = cl_column_tree_model=>eventid_button_click      appl_event = abap_true )
          ( eventid = cl_column_tree_model=>eventid_item_keypress     appl_event = abap_true )
          ( eventid = cl_column_tree_model=>eventid_item_double_click appl_event = abap_true )
          ( eventid = cl_column_tree_model=>eventid_checkbox_change   appl_event = abap_true )
      ).
    ENDIF.

    mr_model->set_registered_events( lt_events ).

    SET HANDLER:
      on_node_context_menu_request FOR mr_model,
      on_node_context_menu_select FOR mr_model,
      on_node_double_click FOR mr_model,
      on_tree_drag FOR mr_model,
      on_tree_drop FOR mr_model,
      on_node_key_press FOR mr_model,
      on_expand_no_children FOR mr_model.

    IF if_single_selection = abap_false.
      SET HANDLER:
        on_tree_drag_multiple FOR mr_model,
        on_tree_drop_complete_multiple FOR mr_model.
    ENDIF.

    IF if_item_selection = abap_true.
      SET HANDLER:
          on_item_button_click FOR mr_model,
          on_item_link_click FOR mr_model,
          on_item_key_press FOR mr_model,
          on_checkbox_change FOR mr_model,
          on_item_double_click FOR mr_model.
    ENDIF.
  ENDMETHOD.


  METHOD on_checkbox_change.
    RAISE EVENT checkbox_change
      EXPORTING
        ev_item_name = item_name
        ef_checked   = checked
        ev_node_key  = node_key.
  ENDMETHOD.


  METHOD on_drop_complete.
    RAISE EVENT drop_complete
      EXPORTING
        ev_node_key         = node_key
        ev_item_name        = item_name
        er_drag_drop_object = drag_drop_object.
  ENDMETHOD.


  METHOD on_expand_no_children.
    RAISE EVENT expand_no_children
      EXPORTING
        ev_node_key = node_key.
  ENDMETHOD.


  METHOD on_item_button_click.
    RAISE EVENT button_click
      EXPORTING
        ev_node_key  = node_key
        ev_item_name = item_name.
  ENDMETHOD.


  METHOD on_item_double_click.
    RAISE EVENT item_double_click
      EXPORTING
        ev_node_key  = node_key
        ev_item_name = item_name.
  ENDMETHOD.


  METHOD on_item_key_press.
    RAISE EVENT item_keypress
      EXPORTING
        ev_node_key  = node_key
        ev_item_name = item_name
        ev_key       = key.
  ENDMETHOD.


  METHOD on_item_link_click.
    RAISE EVENT link_click
      EXPORTING
        ev_node_key  = node_key
        ev_item_name = item_name.
  ENDMETHOD.


  METHOD on_node_context_menu_request.
    RAISE EVENT node_context_menu_request
      EXPORTING
        ev_node_key = node_key
        er_menu     = menu.
  ENDMETHOD.


  METHOD on_node_context_menu_select.
    RAISE EVENT node_context_menu_select
      EXPORTING
        ev_node_key = node_key
        ev_fcode    = fcode.
  ENDMETHOD.


  METHOD on_node_double_click.
    RAISE EVENT node_double_click
      EXPORTING
        ev_node_key = node_key.
  ENDMETHOD.


  METHOD on_node_key_press.
    RAISE EVENT node_keypress
      EXPORTING
        ev_node_key = node_key
        ev_key      = key.
  ENDMETHOD.


  METHOD on_selection_changed.
    RAISE EVENT selection_changed
      EXPORTING
        ev_node_key = node_key.
  ENDMETHOD.


  METHOD on_tree_drag.
    RAISE EVENT drag
      EXPORTING
        ev_node_key         = node_key
        ev_item_name        = item_name
        er_drag_drop_object = drag_drop_object.
  ENDMETHOD.


  METHOD on_tree_drag_multiple.
    RAISE EVENT drag_multiple
      EXPORTING
        et_node_key_table   = node_key_table
        ev_item_name        = item_name
        er_drag_drop_object = drag_drop_object.
  ENDMETHOD.


  METHOD on_tree_drop.
    RAISE EVENT drop
      EXPORTING
        ev_node_key         = node_key
        er_drag_drop_object = drag_drop_object.
  ENDMETHOD.


  METHOD on_tree_drop_complete_multiple.
    RAISE EVENT drop_complete_multiple
      EXPORTING
        et_node_key_table   = node_key_table
        ev_item_name        = item_name
        er_drag_drop_object = drag_drop_object.
  ENDMETHOD.


  METHOD remove_all_key_strokes.
    mr_model->remove_all_key_strokes( ).
  ENDMETHOD.
ENDCLASS.
