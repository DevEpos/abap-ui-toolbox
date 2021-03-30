"! <p class="shorttext synchronized" lang="en">Generic GUI Error</p>
CLASS zcx_uitb_gui_exception DEFINITION
  PUBLIC
  INHERITING FROM zcx_uitb_nc_exception
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS:
      "! <p class="shorttext synchronized" lang="en">Generic Exception message</p>
      BEGIN OF zcx_uitb_gui_exception,
        msgid TYPE symsgid VALUE 'ZUITB_EXCEPTION',
        msgno TYPE symsgno VALUE '011',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_uitb_gui_exception .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcx_uitb_gui_exception IMPLEMENTATION.

ENDCLASS.
