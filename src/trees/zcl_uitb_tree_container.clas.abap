class ZCL_UITB_TREE_CONTAINER definition
  public
  final
  create private .

public section.

  constants C_TREE_SIZE_SMALL type I value 1 ##NO_TEXT.
  constants C_TREE_SIZE_MEDIUM type I value 2 ##NO_TEXT.
  constants C_TREE_SIZE_BIG type I value 3 ##NO_TEXT.

    "! Creates a new tree container which holds
    "! a tree control and an optional toolbar
    "! @parameter if_create_toolbar | controls the creation of the toolbar
    "! @parameter iv_tree_size | controls the size of the tree
    "! @parameter ir_tree_model | the tree model
    "! @parameter rr_container | the resulting container
  class-methods CREATE_CONTAINER
    importing
      !IR_PARENT type ref to CL_GUI_CONTAINER optional
      !IF_CREATE_TOOLBAR type SAP_BOOL optional
      !IV_DOCK_POSITION type I default CL_GUI_DOCKING_CONTAINER=>DOCK_AT_LEFT
      !IV_TREE_SIZE type I default C_TREE_SIZE_SMALL
      !IR_TREE_MODEL type ref to CL_TREE_MODEL
    returning
      value(RR_CONTAINER) type ref to ZCL_UITB_TREE_CONTAINER .
  methods GET_TOOLBAR
    returning
      value(RR_TOOLBAR) type ref to CL_GUI_TOOLBAR .
  methods GET_TREE
    returning
      value(RR_TREE) type ref to CL_GUI_CONTROL .
  methods FREE .
  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS constructor
      IMPORTING
        ir_parent         TYPE REF TO cl_gui_container
        if_create_toolbar TYPE sap_bool
        iv_tree_size      TYPE i
        iv_dock_position  TYPE i
        ir_tree_model     TYPE REF TO cl_tree_model.

    METHODS create_dock.
    METHODS create_toolbar.
    METHODS create_split_screen.
    METHODS get_parent
      RETURNING
        VALUE(result) TYPE REF TO cl_gui_container.
    METHODS init.

    DATA mf_create_toolbar TYPE sap_bool.
    DATA mv_tree_size TYPE i.
    DATA mv_dock_side TYPE i.
    DATA mr_tree_model TYPE REF TO cl_tree_model.
    DATA mr_parent TYPE REF TO cl_gui_container.
    DATA mr_docking_container TYPE REF TO cl_gui_docking_container.
    DATA mr_split_container TYPE REF TO cl_gui_splitter_container.
    DATA mr_tree_container TYPE REF TO cl_gui_container.
    DATA mr_toolbar TYPE REF TO cl_gui_toolbar.
    DATA mr_tree TYPE REF TO cl_gui_control.
ENDCLASS.



CLASS ZCL_UITB_TREE_CONTAINER IMPLEMENTATION.


  METHOD constructor.
    mr_parent = ir_parent.
    mf_create_toolbar = if_create_toolbar.
    mr_tree_model = ir_tree_model.
    mv_dock_side = iv_dock_position.
    mv_tree_size = iv_tree_size.
  ENDMETHOD.


  METHOD create_container.
    rr_container = NEW #(
        if_create_toolbar = if_create_toolbar
        ir_tree_model     = ir_tree_model
        iv_dock_position  = iv_dock_position
        iv_tree_size      = iv_tree_size
        ir_parent         = ir_parent
    ).

    rr_container->init( ).
  ENDMETHOD.


  METHOD create_dock.
    CHECK mr_docking_container IS INITIAL.

    mr_docking_container = NEW cl_gui_docking_container(
        side                        = mv_dock_side
        extension                   = SWITCH #(
            mv_tree_size
            WHEN c_tree_size_big THEN
                800
            WHEN c_tree_size_medium THEN
                600
            WHEN c_tree_size_small THEN
                400
        )
    ).
  ENDMETHOD.


  METHOD create_split_screen.
    CHECK mr_split_container IS INITIAL.

    mr_split_container = NEW cl_gui_splitter_container(
        parent  = get_parent( )
        columns = 1
        rows    = 2
    ).

    mr_split_container->set_row_sash(
      id    = 1
      type  = cl_gui_splitter_container=>type_movable
      value = cl_gui_splitter_container=>false
    ).
    mr_split_container->set_row_sash(
      id    = 1
      type  = cl_gui_splitter_container=>type_sashvisible
      value = cl_gui_splitter_container=>false
    ).

    mr_split_container->set_row_mode( cl_gui_splitter_container=>mode_absolute ).
    mr_split_container->set_row_height( id = 1 height = 25 ).
  ENDMETHOD.


  METHOD create_toolbar.
    CHECK mr_toolbar IS INITIAL.
    CHECK mr_split_container IS NOT INITIAL.

    DATA(lr_top_container) = mr_split_container->get_container(
        row       = 1
        column    = 1
    ).

    mr_toolbar = NEW cl_gui_toolbar(
        parent             = lr_top_container
    ).
  ENDMETHOD.


  METHOD free.
    IF mr_docking_container IS BOUND.
      mr_docking_container->free( ).
      CLEAR mr_docking_container.
    ENDIF.
  ENDMETHOD.


  METHOD get_parent.
    result = COND #( WHEN mr_parent IS BOUND THEN mr_parent ELSE mr_docking_container ).
  ENDMETHOD.


  METHOD get_toolbar.
    rr_toolbar = mr_toolbar.
  ENDMETHOD.


  METHOD get_tree.
    IF mr_tree IS INITIAL.
      mr_tree_model->create_tree_control(
        EXPORTING parent  = mr_tree_container
        IMPORTING control = DATA(lr_tree_control)
      ).

      mr_tree = lr_tree_control.
    ENDIF.

    rr_tree = mr_tree.
  ENDMETHOD.


  METHOD init.
    IF mr_parent IS INITIAL.
      create_dock( ).
    ENDIF.

    IF mf_create_toolbar = abap_true.
      create_split_screen( ).
      create_toolbar( ).
      mr_tree_container = mr_split_container->get_container(
          row    = 2
          column = 1
      ).
    ELSE.
      mr_tree_container = get_parent( ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
