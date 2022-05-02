System.out.println("Loading net...");


net_and_marking = import_petri_net_from_pnml_file("net.pnml");
net = net_and_marking[0];

System.out.println("Running reduction...");

result_net = reduce_all_transitions_retain_sink_source_places(net);

File net_file = new File("reduced.pnml");

pnml_export_petri_net_(result_net, net_file);
System.exit(0);