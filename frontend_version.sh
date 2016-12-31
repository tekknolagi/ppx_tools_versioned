#!/bin/sh
VAR=$(grep cmi_magic_number | cut -f 2 -d ' ')
case "$VAR" in
  Caml1999I017)
    echo "402"
  ;;
  Caml1999I020)
    echo "403"
  ;;
  Caml1999I021)
    echo "404"
  ;;
  *)
    exit 1
  ;;
esac  
