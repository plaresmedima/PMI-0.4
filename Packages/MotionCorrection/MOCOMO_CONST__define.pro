FUNCTION MOCOMO_CONST::FIT_SIGNAL, S

  RETURN, S*0 + total(S)/n_elements(S)

END


PRO MOCOMO_CONST::SET_MODEL, X
END


PRO MOCOMO_CONST__DEFINE

  struct = {MOCOMO_CONST, Dummy:0B}

END