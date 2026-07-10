module {
  func.func @saxpy(%a: f32, %x: tensor<4xf32>, %y: tensor<4xf32>) -> tensor<4xf32> {
    %0 = arith.mulf %x, %a : tensor<4xf32>
    %1 = arith.addf %0, %y : tensor<4xf32>
    "test.consume"(%1) {message = "result"} : (tensor<4xf32>) -> ()
    return %1 : tensor<4xf32>
  }
}
