import json
import sys

# Render input.json:
# $ python3 path/to/json-to-digraph.py input.json | python3 -m xdot -

with open(sys.argv[1], "r") as fp:
    parsed = json.load(fp)

    print("digraph {rankdir=LR;")

    for i,v in enumerate(parsed["passes"]):
        v["index"] = i + 1
        print("P{index} [label=\"{name} ({writeCount})\"".format(**v), end="")
        if v["sideEffect"]:
            print(",fillcolor=\"#ff4b00\",style=filled,shape=oval];")
        elif v["references"] > 0:
            print(",fillcolor=\"#64d50a\",style=filled,shape=oval];")
        else:
            print(",color=\"#64d50a\",style=solid,shape=oval];")

    for i,v in enumerate(parsed["resources"]):
        v["index"] = i + 1
        print("R{index} [label=\"{name} v{version}\n{kind}\n{format} ({readCount})\"".format(**v), end="")
        if v["references"] > 0:
            if v["implicit"]:
                print(",fillcolor=\"#87baf7\",style=dashed,shape=box];")
            else:
                print(",fillcolor=\"#2480f1\",style=filled,shape=box];")
        else:
            if v["implicit"]:
                print(",color=\"#87baf7\",style=dashed,shape=box];")
            else:
                print(",color=\"#2480f1\",style=solid,shape=box];")

    for v in parsed["writes"]:
        v["source"] += 1
        v["target"] += 1
        print("P{source}->R{target} [color=\"#cb4646\"];".format(**v))
    for v in parsed["reads"]:
        v["source"] += 1
        v["target"] += 1
        print("R{source}->P{target} [color=\"#75cb46\"];".format(**v))

    print("}")
