pro keV_to_wave_length, energy_keV

  ; energy in keV to wave length in metre

  wave_len = 6.62606896e-34*299792458/(energy_keV*1.60217733e-16) 
end

pro phase_filter

  dim0 = 3
  dim1 = 4

  arr0 = indgen(dim1) # replicate(1, dim0)
  arr1 = replicate(1, dim1) # indgen(dim0)
  
  
  print, 'meshgrid dim0:'
  print, arr0
  print, 'meshgrid dim1:' 
  print, arr1

end

pro phase_est

  energy = 20 ; photon energy in keV
  wave_len = 6.62606896e-34*299792458/(energy*1.60217733e-16) ; energy in keV to wave length in metre
  pixel_size = 1.3e-6 ; effective pixel size in metre
  
  z_max = 4 * pixel_size^2 / wave_len ; first zero CTF sine function in metre
  
  print, 'E = ', energy, ' keV'
  print, 'lambda =', wave_len * 1e10, ' Angstrom'
  print, 'dx = ', pixel_size * 1e6, ' micron'
  print, '1st maximum of pure-phase contrast transfer function'
  print, 'z = ', z_max * 1e3, ' mm'
  
  ;phase_filter
  lin_err = 0.1
  N = 1000.
  x = [1:N-1]/N
  pos = (where(abs(sin(x)-x)/abs(sin(x)) gt lin_err))[0]
  print, pos, x[pos - 1]
  
end

