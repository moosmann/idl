pro test_filter_pixel_hot

  compile_opt idl2

  on_error, 2
  
  ; expected image
  expected = make_array(5, 5, /float, value=1)
  im_size = size(expected, /dimensions)
  
  ; image with hot pixel
  hot = expected
  hot[im_size / 2] = 2
  
  ; apply pixel filter
  result = filter_pixel(hot)
  
  if ~array_equal(result, expected) then begin
        
    ind = where(result ne expected)
    message, 'Hot pixel test failed: position' + string(ind) + ' should be' + string(result[ind]) +  ' but is' + string(expected[ind]) 
      
  endif
  
end


pro test_filter_pixel_nan

  compile_opt idl2

  on_error, 2

  ; expected image
  expected = make_array(5, 5, /float, value=1)
  im_size = size(expected, /dimensions)

  ; image with hot pixel
  hot = expected
  hot[im_size / 2] = !values.F_NAN

  ; apply pixel filter
  result = hot

  if ~array_equal(result, expected) then begin

    ind = where(result ne expected)
    message, 'NAN pixel test failed: position' + string(ind) + ' should be' + string(result[ind]) +  ' but is' + string(expected[ind])

  endif

end



pro test_filter_pixel

  compile_opt idl2
  
  print

  print, 'Testing suite for filter_pixel()'

end