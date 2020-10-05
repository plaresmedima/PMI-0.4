


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;		AUXILIARY                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


PRO MoCoModel_OneCompartmentDelay::ShiftAif, Delay

  ca = ShiftAif(*self.ca,*self.t,Delay)
  n = n_elements(ca)
  dt = *self.dt
  ca1 = [0, TOTAL( dt*(ca[0:n-2]+ca[1:n-1]), /cumulative)]
  (*self.matrix)[1,*] = ca1

END


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;		REQUIRED                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


FUNCTION MoCoModel_OneCompartmentDelay::PIXEL_PARAMETERS, S, FIT=Fi

  dDelay = 0.25 ;sec
  maxDelay = 5.0 ;sec
  nDelay = 1+ceil(maxDelay/dDelay)
  Delay = dDelay*findgen(nDelay)
  Error = fltarr(nDelay)

  for i=0L, nDelay-1 do begin
    self -> ShiftAif, Delay[i]
    Pi = self -> MoCoModel_OneCompartment::PIXEL_PARAMETERS(S, FIT=Fi)
    Error[i] = total((S-Fi)^2)
  endfor
  tmp = min(Error,i)
  self -> ShiftAif, Delay[i]
  Pi = self -> MoCoModel_OneCompartment::PIXEL_PARAMETERS(S, FIT=Fi)

  P = FLTARR(self.nr_of_free_parameters)
  P[self.nr_of_free_parameters-1] = Delay[i]
  P[0:self.nr_of_free_parameters-2] = Pi

  RETURN, P

END


FUNCTION MoCoModel_OneCompartmentDelay::PIXEL_FORWARD, P, S

  self -> ShiftAif, P[self.nr_of_free_parameters-1]
  RETURN, self->MoCoModel_OneCompartment::PIXEL_FORWARD(P[0:self.nr_of_free_parameters-2], S)

END


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;		CLEANUP, INIT and DEFINE  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


PRO MoCoModel_OneCompartmentDelay::CLEANUP

  self->MoCoModel_OneCompartment::CLEANUP
  ptr_free, self.t, self.ca
END


FUNCTION MoCoModel_OneCompartmentDelay::INIT, fixed_parameters

  OK = self->MoCoModel_OneCompartment::INIT(fixed_parameters)

  self.nr_of_free_parameters = 3
  self.t = ptr_new(fixed_parameters.t)
  self.ca = ptr_new(fixed_parameters.ca)

  RETURN, OK
END


PRO MoCoModel_OneCompartmentDelay__DEFINE

  struct = {$
    MoCoModel_OneCompartmentDelay, $
    INHERITS MoCoModel_OneCompartment, $
    t: ptr_new(), $
    ca: ptr_new() $
  }

END