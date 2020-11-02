#{prefix => "",
  security => false,
  routes => [
            {"/", { ldf_main_controller, index}, #{methods => [get]}}
           ],
 statics => [
             {"/assets/[...]", "assets"}
            ]
}.
