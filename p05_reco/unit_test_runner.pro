; Path  path to test directory

pro unit_test_runner, path

  compile_opt idl2



  if ~file_test(path, /directory) then begin

    message, 'Input must be a path.'

  endif



  test_files = file_search(path, 'test*.pro')

  resolve_routine, file_basename(test_files,'.pro'), /compile_full_file

  tests = routine_info()



  print

  print,'--------------------------------------------------------------------------------'



  error_count = 0

  for i=0, tests.length-1 do begin

    catch, errorStatus

    if (errorStatus ne 0) then begin

      catch, /cancel

      print, 'ERROR: ', !ERROR_STATE.msg

      i++

      error_count++

      continue

    endif



    if (tests[i]).startswith('TEST_') then begin

      call_procedure, tests[i]

    endif

  endfor



  print

  print,'--------------------------------------------------------------------------------'

  print



  if error_count gt 0 then begin

    print, 'Unit test failures on: ' + path

  endif else begin

    print, 'Unit tests pass.'

  endelse



end