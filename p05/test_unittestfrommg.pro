function gpurepmat_ut::test_1d
  ; do testing
  return, 1
end

function gpurepmat_ut::test_2d
  ; do testing
  return, 1
end

pro gpurepmat_ut__define
  define = { gpurepmat_ut, inherits GPUutTestCase }
  end
