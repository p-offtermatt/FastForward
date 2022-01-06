System.out.println("Loading net...");


net_and_marking = import_petri_net_from_pnml_file("net.pnml");
net = net_and_marking[0];

System.out.println("Running woflan...");

start_time = System.nanoTime();
diagnosis = analyze_with_woflan(net);
end_time = System.nanoTime();

System.out.println(diagnosis);

System.out.println("Total Woflan time in millis:");
System.out.println((float)(end_time - start_time)/1000000);
System.exit(0);