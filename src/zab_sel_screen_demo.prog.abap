*&---------------------------------------------------------------------*
*& Report ZAB_SEL_SCREEN_DEMO
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zab_sel_screen_demo.

TABLES:
  sflight.

INCLUDE zab_selscreen_controller.

CLASS lcl_app DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-DATA:
      go_screen_carr TYPE REF TO lcl_sel_screen,
      go_screen_conn TYPE REF TO lcl_sel_screen.
    CLASS-METHODS:
      create_screens,
      call_sh,
      pbo.
ENDCLASS.

PARAMETERS: r1 RADIOBUTTON GROUP rad DEFAULT 'X' USER-COMMAND state,
            r2 RADIOBUTTON GROUP rad.

SELECT-OPTIONS: so_carr FOR sflight-carrid,
                so_conn FOR sflight-connid.

INITIALIZATION.
  lcl_app=>create_screens( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR so_conn-low.
  lcl_app=>call_sh( ).

AT SELECTION-SCREEN OUTPUT.
  lcl_app=>pbo( ).

CLASS lcl_app IMPLEMENTATION.

  METHOD create_screens.
    go_screen_carr = NEW #( ).
    go_screen_carr->set_text( iv_field = 'SO_CARR' iv_text  = 'Carrier ID' ).
    go_screen_carr->enable_options( iv_field   = 'SO_CARR' iv_options = 'EQ' ).
    go_screen_carr->set_active( iv_field = 'SO_CONN' iv_state = abap_false ).
    go_screen_carr->set_text( iv_field = 'R1' iv_text = 'State 1' ).
    go_screen_carr->set_text( iv_field = 'R2' iv_text = 'State 2' ).
    go_screen_carr->set_btn(
      EXPORTING
        iv_id   = 1
        iv_text = 'First btn'
        iv_icon = icon_led_green
        iv_info = 'First btn' ).
    go_screen_carr->exclude_command( 'ONLI' ).

    go_screen_conn = NEW #( ).
    go_screen_conn->set_text( iv_field = 'SO_CONN' iv_text  = 'Conn ID' ).
    go_screen_conn->enable_options( iv_field   = 'SO_CONN' iv_options = 'BT EQ' ).
    go_screen_conn->set_active( iv_field = 'SO_CARR' iv_state = abap_false ).
    go_screen_conn->set_text( iv_field = 'R1' iv_text = 'State 1' ).
    go_screen_conn->set_text( iv_field = 'R2' iv_text = 'State 2' ).
    go_screen_conn->set_btn(
      EXPORTING
        iv_id   = 1
        iv_text = 'Second btn'
        iv_icon = icon_led_red
        iv_info = 'Second btn' ).
  ENDMETHOD.

  METHOD call_sh.
    DATA:
      ls_shlp_descr TYPE shlp_descr,
      lv_rc         LIKE sy-subrc,
      lt_values     TYPE STANDARD TABLE OF ddshretval.

    CALL FUNCTION 'F4IF_GET_SHLP_DESCR'
      EXPORTING
        shlpname = 'H_SPFLI'
        shlptype = 'SH'
      IMPORTING
        shlp     = ls_shlp_descr.

    DATA(lv_value) = lcl_sel_screen=>get_value( iv_field = 'SO_CONN-LOW' ).

    ls_shlp_descr-interface[ shlpfield = 'CONNID': ]-valfield = abap_true,
                                                   ]-value    = lv_value.

    CALL FUNCTION 'F4IF_START_VALUE_REQUEST'
      EXPORTING
        shlp          = ls_shlp_descr
        disponly      = abap_false
        maxrecords    = 0
        multisel      = space
      IMPORTING
        rc            = lv_rc
      TABLES
        return_values = lt_values.

    so_carr-low = VALUE #( lt_values[ fieldname = 'CARRID' ]-fieldval OPTIONAL ).
  ENDMETHOD.

  METHOD pbo.
    CASE abap_true.
      WHEN r1.
        go_screen_carr->show( ).
      WHEN r2.
        go_screen_conn->show( ).
    ENDCASE.
  ENDMETHOD.

ENDCLASS.
