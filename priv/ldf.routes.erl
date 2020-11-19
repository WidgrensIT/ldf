#{prefix => "",
  security => false,
  routes => [
            {"/receiver", { ldf_receiver_controller, incoming_message}, #{methods => [post]}}
           ],
 statics => [
             {"/assets/[...]", "assets"}
            ]
}.