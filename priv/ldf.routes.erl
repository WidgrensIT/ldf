#{prefix => "",
  security => false,
  routes => [
            {"/receiver", { ldf_receiver_controller, incoming_message}, #{methods => [post]}},
            {"/li", {ldf_li_controller, manage_li}, #{methods => [post, get]}},
            {"/li/:liid", {ldf_li_controller, delete_li}, #{methods => [delete]}}
           ],
 statics => [
             {"/assets/[...]", "assets"}
            ]
}.