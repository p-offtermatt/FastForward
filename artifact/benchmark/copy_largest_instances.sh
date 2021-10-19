for file in $(cat largest_instances.txt); do cp nets/wahl-kroening/$file* nets/largest_instances; done
for file in $(cat largest_instances.txt); do cp nets/soter/$file* nets/largest_instances; done
for file in $(cat largest_instances.txt); do cp nets/mist/PN/$file* nets/largest_instances; done
for file in $(cat largest_instances.txt); do cp nets/mist/boundedPN/$file* nets/largest_instances; done
for file in $(cat largest_instances.txt); do cp nets/medical/$file* nets/largest_instances; done
for file in $(cat largest_instances.txt); do cp nets/bug_tracking/$file* nets/largest_instances; done
