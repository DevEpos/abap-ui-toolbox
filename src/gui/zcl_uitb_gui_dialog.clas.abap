"! <p class="shorttext synchronized" lang="en">Non modal generic dialog with optional toolbar</p>
CLASS zcl_uitb_gui_dialog DEFINITION
  PUBLIC
  INHERITING FROM zcl_uitb_gui_screen_base
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.
    "! <p class="shorttext synchronized" lang="en">Creates new Generic non modal dialog</p>
    "!
    METHODS constructor
      IMPORTING
        iv_title TYPE string.
    METHODS zif_uitb_gui_screen~show
        REDEFINITION.
    METHODS zif_uitb_gui_screen~leave_screen
        REDEFINITION.
  PROTECTED SECTION.
    METHODS create_container
        REDEFINITION.
  PRIVATE SECTION.
    DATA mo_dialog_box TYPE REF TO cl_gui_dialogbox_container.
    "! <p class="shorttext synchronized" lang="en">Close handler for dialog box container</p>
    "!
    METHODS on_close
        FOR EVENT close OF cl_gui_dialogbox_container .
ENDCLASS.



CLASS zcl_uitb_gui_dialog IMPLEMENTATION.
  METHOD constructor.
    super->constructor(
        iv_title    = iv_title
    ).

    mf_as_dialog = abap_true.
  ENDMETHOD.

  METHOD zif_uitb_gui_screen~show.
    DATA: ls_dialog_area TYPE zcl_uitb_screen_util=>ty_s_area.

    IF iv_top IS INITIAL AND iv_left IS INITIAL.
      ls_dialog_area = zcl_uitb_screen_util=>get_screen_area_for_size(
         is_size   = VALUE #( metric = zcl_uitb_screen_util=>c_metrics-pixel height = iv_height width = iv_width )
         iv_metric = zcl_uitb_screen_util=>c_metrics-pixel
      ).
    ELSE.
      ls_dialog_area = VALUE #(
         top  = iv_top
         left = iv_left
         right = iv_left + iv_width
         bottom = iv_top + iv_height
      ).
    ENDIF.

    mo_dialog_box = NEW cl_gui_dialogbox_container(
        width   = ls_dialog_area-right - ls_dialog_area-left
        height  = ls_dialog_area-bottom - ls_dialog_area-top
        top     = ls_dialog_area-top
        left    = ls_dialog_area-left
        metric  = cl_gui_control=>metric_pixel
        caption = |{ mv_title }|
        style   =  cl_gui_control=>ws_thickframe +
                   cl_gui_control=>ws_minimizebox +
                   cl_gui_control=>ws_maximizebox +
                   cl_gui_control=>ws_sysmenu
    ).
    SET HANDLER: on_close FOR mo_dialog_box.
    mo_container = mo_dialog_box.

    create_content( mo_container ).

    cl_gui_cfw=>flush( ).
    mo_dialog_box->set_visible( cl_gui_control=>visible_true ).

*.. No PBO so flag has to be set here
    mf_visible = abap_true.
  ENDMETHOD.

  METHOD zif_uitb_gui_screen~leave_screen.
    on_close( ).
  ENDMETHOD.

  METHOD on_close.
    mo_dialog_box->set_visible( cl_gui_control=>visible_false ).
    mo_dialog_box->free( ).

    do_after_exit( ).
  ENDMETHOD.

  METHOD create_container ##needed.
*.. Container will also be the screen
  ENDMETHOD.

ENDCLASS.
