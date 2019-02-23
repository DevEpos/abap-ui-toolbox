*&---------------------------------------------------------------------*
*& Report zuitb_gui_framework_test
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zuitb_gui_framework_test.

CLASS cl_view DEFINITION
INHERITING FROM zcl_uitb_gui_screen.

  PUBLIC SECTION.
    METHODS constructor.
    METHODS zif_uitb_gui_command_handler~execute_command
        REDEFINITION.
  PROTECTED SECTION.
    METHODS create_content
        REDEFINITION.
    METHODS do_before_dynpro_output
        REDEFINITION.
    METHODS create_tree.
    METHODS create_alv.
  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF c_control_id,
        sort      TYPE string VALUE 'SORT',
        output    TYPE string VALUE 'OUTPUT',
        selection TYPE string VALUE 'SELECTION',
      END OF c_control_id.
    DATA mo_splitter TYPE REF TO zcl_uitb_gui_splitter_cont.
    data mo_alv_splitter type ref to zcl_uitb_gui_splitter_cont.
    DATA mo_switch TYPE REF TO zcl_uitb_gui_switch_container.
    DATA mo_alv TYPE REF TO zcl_uitb_alv.
    DATA mt_dummy_data TYPE zuitb_generic_range_itab.
    DATA mo_tree TYPE REF TO zcl_uitb_column_tree_model.
ENDCLASS.

CLASS cl_view IMPLEMENTATION.
  METHOD create_alv.
    mo_alv_splitter = new zcl_uitb_gui_splitter_cont(
      iv_elements = 2
      iv_size     = '70:30'
      iv_mode     = zcl_uitb_gui_splitter_cont=>c_mode-rows
      io_parent   = mo_switch->add_child( c_control_id-output )
    ).
    mo_alv_splitter->set_element_visibility( iv_element = 2 if_visible = abap_false ).
    mo_alv = zcl_uitb_alv=>create_alv(
       ir_data                 = REF #( mt_dummy_data )
       ir_container            = mo_alv_splitter->get_container( 1 )
       if_editable             = abap_true
    ).
    mo_alv->get_functions( )->set_default( ).
    mo_alv->get_columns( )->set_editable( ).

    mo_alv->display( ).

    data(lo_alv2) = zcl_uitb_alv=>create_alv(
      ir_data      = ref #( mt_dummy_data )
      ir_container = mo_alv_splitter->get_container( 2 )
    ).
    lo_alv2->get_functions( )->set_default( ).
    lo_alv2->display( ).
  ENDMETHOD.

  METHOD create_tree.
    create_control_toolbar(
      EXPORTING
        io_parent  = mo_splitter->get_container( 1 )
        it_button  = VALUE #(
          ( function = 'EXP'  icon = icon_expand_all )
          ( function = 'COLL' icon = icon_collapse_all )
          ( butn_type = cntb_btype_sep )
          ( function = 'SORTM' text = 'Sorting'   icon = icon_sort_up checked = abap_true )
          ( function = 'OUTPM' text = 'Output'    icon = icon_table_settings )
          ( function = 'SELM'  text = 'Selection' icon = icon_align_left  )
        )
      IMPORTING
        eo_toolbar = DATA(lo_toolbar)
        eo_client  = DATA(lo_container)
    ).

*.. Create tree
    mo_tree = NEW zcl_uitb_column_tree_model(
      ir_parent           = lo_container
      is_hierarchy_header = VALUE #(
        heading = 'Object' width = 60
      )
      if_auto_node_key    = abap_true
      iv_selection_mode   = zcl_uitb_column_tree_model=>c_multiple_selection
    ).

    DATA(lo_nodes) = mo_tree->get_nodes( ).
    DATA(lo_folder_node) = lo_nodes->add_node(
        if_folder            = abap_true
        it_item_table        = VALUE #(
          ( class = cl_item_tree_model=>item_class_text text = 'Entity' item_name = zcl_uitb_column_tree_model=>c_hierarchy_column )
        )
    ).
    DO 10 TIMES.
      DATA(lo_child) = lo_nodes->add_node(
        iv_relative_node_key = lo_folder_node->mv_node_key
        it_item_table        = VALUE #(
          ( class = cl_item_tree_model=>item_class_text text = |Field { sy-index }| item_name = zcl_uitb_column_tree_model=>c_hierarchy_column )
        )
      ).
    ENDDO.

    mo_tree->create_tree_control( ).
    mo_tree->get_nodes( )->set_first_root_node_as_top( ).
    mo_tree->get_nodes( )->expand_root_nodes( ).
  ENDMETHOD.

  METHOD create_content.
    mo_splitter = NEW zcl_uitb_gui_splitter_cont(
      iv_elements  = 2
      iv_size      = '30:70'
      iv_mode      = zcl_uitb_gui_splitter_cont=>c_mode-cols
      io_parent    = io_container
    ).
    mo_switch = NEW #( mo_splitter->get_container( 2 ) ).
    create_tree( ).
    create_alv( ).
  ENDMETHOD.

  METHOD constructor.

    super->constructor( iv_title = 'Test Screen' ).

  ENDMETHOD.

  METHOD zif_uitb_gui_command_handler~execute_command.
    CASE io_command->mv_function.

      when 'EXP'.
        mo_alv_splitter->set_element_visibility( iv_element = 2 ).

      when 'COLL'.
        mo_alv_splitter->set_element_visibility( iv_element = 1 ).

      WHEN 'SORTM'.
        mo_switch->set_child_visible( iv_id = c_control_id-output if_visible = abap_false ).

      WHEN 'OUTPM'.
        mo_switch->set_child_visible( iv_id = c_control_id-output if_visible = abap_true ).

      WHEN 'SELM'.
        mo_splitter->set_element_visibility( iv_element = 2 ).

    ENDCASE.
  ENDMETHOD.

  METHOD do_before_dynpro_output.
    io_callback->map_fkey_function(
        iv_fkey            = zif_uitb_c_gui_screen=>c_functions-f5
        iv_mapped_function = 'EXP'
        iv_text            = 'Some Function'
    ).
  ENDMETHOD.

ENDCLASS.


START-OF-SELECTION.
  NEW cl_view( )->zif_uitb_gui_screen~show( ).
