FUNCTION-POOL zuitb_gui_framework.          "MESSAGE-ID ..

INCLUDE lzuitb_gui_frameworkd01.

CONSTANTS: gc_max_function_number TYPE i VALUE 28.

DATA ok_code TYPE sy-ucomm.

DATA: BEGIN OF gs_view_data,
        controller    TYPE REF TO zcl_uitb_gui_dynpro_handler,
        status_prog   TYPE sy-repid,
        fkey_map      TYPE zif_uitb_ty_gui_screen=>ty_t_fkey_map,
        status        TYPE syst_pfkey,
        is_first_call TYPE abap_bool,
        title         TYPE syst_title,
        as_dialog     TYPE abap_bool,
      END OF gs_view_data.

DATA gt_view_controller LIKE STANDARD TABLE OF gs_view_data.

DATA: BEGIN OF gs_function,
        f2      TYPE smp_dyntxt,
        f5      TYPE smp_dyntxt,
        f6      TYPE smp_dyntxt,
        f7      TYPE smp_dyntxt,
        f8      TYPE smp_dyntxt,
        f9      TYPE smp_dyntxt,
        s_f1    TYPE smp_dyntxt,
        s_f2    TYPE smp_dyntxt,
        s_f4    TYPE smp_dyntxt,
        s_f5    TYPE smp_dyntxt,
        s_f6    TYPE smp_dyntxt,
        s_f7    TYPE smp_dyntxt,
        s_f8    TYPE smp_dyntxt,
        s_f9    TYPE smp_dyntxt,
        s_f11   TYPE smp_dyntxt,
        s_f12   TYPE smp_dyntxt,
        c_f1    TYPE smp_dyntxt,
        c_f2    TYPE smp_dyntxt,
        c_f3    TYPE smp_dyntxt,
        c_f4    TYPE smp_dyntxt,
        c_f5    TYPE smp_dyntxt,
        c_f6    TYPE smp_dyntxt,
        c_f7    TYPE smp_dyntxt,
        c_f8    TYPE smp_dyntxt,
        c_f9    TYPE smp_dyntxt,
        c_f10   TYPE smp_dyntxt,
        c_f11   TYPE smp_dyntxt,
        c_f12   TYPE smp_dyntxt,
        c_s_f2  TYPE smp_dyntxt,
        c_s_f3  TYPE smp_dyntxt,
        c_s_f4  TYPE smp_dyntxt,
        c_s_f5  TYPE smp_dyntxt,
        c_s_f6  TYPE smp_dyntxt,
        c_s_f7  TYPE smp_dyntxt,
        c_s_f8  TYPE smp_dyntxt,
        c_s_f9  TYPE smp_dyntxt,
        c_s_f10 TYPE smp_dyntxt,
        c_s_f11 TYPE smp_dyntxt,
        c_s_f12 TYPE smp_dyntxt,
      END OF gs_function.
