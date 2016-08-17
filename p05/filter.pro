function filter_pixel, img, kernel_width=kernel_width

  
  ; Default arguments
  if (n_elements(kernel_width) eq 0) then kernel_width = 3
  
  ; Kernel width
  ndim_img = size(img, /dimensions)
  ndim_kernel = size(kernel_width, /dimensions)
  ; if kernel_width is scalar use symmetric kernel
  if ((ndim_img gt ndim_kernel) and (ndim_kernel eq 1)) then kernel_width = make_array(ndim_img, value=kernel_width)

  img=reform(img)
  
  ; Create median filtered image with above kernel size
  img_med = median(img, kernel_width)

  ; Check for INFs and NANs
  vec = where(~finite(img))
  
  ; Filter
  img(mask) = img_med
  
  foreach element, nan_list do begin
    if element eq 0 or element eq n_elements(img)-1 then img(element)=0 else img(element)=mean([img(element-1),img(element+1)],/nan)
  endforeach

  return, img
end


function pad_image, im, width, method, value=value, type_code=type_code

  ; Pad input image 'im' 
  ; WIDTH: 
  ;   if number of elements is 1, image is padded with WIDTH elements before and after for each dimension.
  ;   if number of elements is 2, image is padded with WIDTH[0] and WIDTH[1] elements before and after for dimension 0 and 1, respectively.
  ;   if number of elements is 4, image is padded with WIDTH[0] and WIDTH[1] elements along dimension 0 before and after, respectively, 
  ;     and with WIDTH[2] and WIDTH[3] elements along dimension 1 before and after, respectively.
  ; METHOD:
  ;   'constant': pad with VALUE or 0 if VALUE is not given.
  ;   'symmetric': pad symmetrically on the edges i.e. the values are mirrored on the edges. 
  ;   'edge': replicate value on edges.
  ; VALUE: value used for constant padding. If undefined VALUE defaults to zero.
  
  im_size = size(im, /dimensions)
  im_ndim = size(im, /n_dimensions)
  if n_elements(type_code) eq 0 then type_code = size(im, /type)
  
  case n_elements(width) of
    1: begin
      w0_before = width
      w0_after = width
      w1_before = width
      w1_after = width
      end
    2: begin
      w0_before = width[0]
      w0_after = w0_before
      w1_before = width[1]
      w1_after = w1_before
      end
    4: begin
      w0_before = width[0]
      w0_after = width[1]
      w1_before = width[2]
      w1_after = width[3]      
      end
  else: message, 'Number of elements of WIDTH not in 1, 2, or 4.'
  endcase
  
  ; padded dimensions
  pad_size = im_size + [w0_before + w0_after, w1_before + w1_after]
  
  
  if method eq 'constant' then begin
    
    ; If empty VALUE defaults to 0
    if n_elements(value) eq 0 then value = 0  
    
    ; Create constant image
    type_code = max([size(value, /type), type_code])
    im_pad = make_array(pad_size, value=value, type=type_code)
    
    ; Fill interior with input image
    im_pad[w0_before:-w0_after - 1, w1_before:-w1_after - 1] = im
    
  endif
  
  
  if method eq 'symmetric' then begin
    
    
    if (max([w0_before, w0_after]) ge im_size[0]) or (max([w1_before, w1_after]) ge im_size[1]) then begin
      message, 'Width of padded area greather than input image.'
    endif
    
    ; Create zero image
    im_pad = make_array(pad_size, type=type_code)
    
    ; 1st dimension before, interior, after
    im_pad[*, w1_before:-w1_after - 1] = [im[w0_before:1:-1, *], im[*, *], im[-2:-w0_after - 1:-1, *]]
    
    ; 2nd dimension before
    im_pad[*, 0:w1_before - 1] = im_pad[*, 2 * w1_before:w1_before + 1:-1]
    ; 2nd dimension after
    im_pad[*, -w1_after:-1] = im_pad[*, -w1_after - 2 :-2 * w1_after - 1:-1]
     
  endif
  

  if method eq 'edge' then begin
    
    ; Create zero image
    im_pad = make_array(pad_size, type=type_code)

    ; Fill interior with input image
    im_pad[w0_before:-w0_after - 1, w1_before:-w1_after - 1] = im
    
    ; 1st dimension before
    im_pad[0:w0_before - 1, w1_before:-w1_after - 1] = make_array(w0_before, value=1) # im[0, *]
    
    ; 1st dimension after
    im_pad[-w0_after:-1, w1_before:-w1_after - 1] = make_array(w0_after, value=1) # im[-1, *]
    
    ; 2nd dimension before
    im_pad[*, 0:w1_before - 1] = im_pad[*, w1_before] # make_array(w1_before, value=1)
    
    ; 2nd dimension after
    im_pad[*, -w1_after:-1] = im_pad[*, -w1_after - 1] # make_array(w1_after, value=1)

  endif
  
  return, im_pad
  
end

