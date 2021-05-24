import pydot, re, sys

dpath="project/static/diagram1.png"

# constraint network from racket
# can replace the cmdline arg with test strings below to debug
constraint_network = sys.argv[1]

# TESTS

# Basic
# "((adder ans l2 l1) (constant a l1) (constant b l2))"
# "((adder l1 l2 top) (constant a l1) (constant b l2) (adder l3 ans top) (constant c l3))"

# Complex
# "((powerer l1 l2 top) (constant a l1) (adder l2 l4 l3) (constant b l3) (constant c l4) (adder ans l5 top) (constant d l5))"
# "((multiplier ans l2 l1) (powerer l3 l4 l1) (adder l3 l6 l5) (powerer l7 l8 l5) (constant 8 l7) (constant 2 l8) (multiplier l9 l10 l6) (constant 4 l9) (multiplier l11 l12 l10) (constant 2 l11) (constant 3 l12) (multiplier l4 l14 l13) (constant 1 l13) (constant 2 l14) (constant 4 l2))"
# "((adder l1 l2 top) (multiplier l1 l4 l3) (constant 3 l3) (multiplier l5 l6 l4) (constant 8 l5) (multiplier l7 l8 l6) (constant -1 l7) (constant 19 l8) (powerer l9 l10 l2) (constant 5 l9) (powerer l11 l10 l12) (constant 9 l11) (constant 81 l12) (multiplier ans l13 top) (multiplier l14 l15 l13) (constant -1 l14) (adder l16 l17 l15) (constant 7 l16) (constant 12 l17))"

# parenthesized blocks
constraints = constraint_network[1:-1]

# dict used for conversion from constraint type to symbol for graph
constraint_symbols = {  "adder": "+",
                        "multiplier": "*",
                        "powerer": "^",
}

# define undirected graph (global)
G = pydot.Dot(graph_type="graph")

def main():
    # manually add the "ans" (unknown var) node
    add_const_nodes(constraints)
    add_nonconst_nodes(constraints)
    add_edges(constraints)
    add_ans_node()

    G.write(path=dpath, format="png")

# add all constant constraints to graph
# constant nodes are circle and named after the connector
def add_const_nodes(constraints):
    # iter through parenthesized blocks (constraint lists)
    for paren in re.findall("\((.*?)\)", constraints):
        members = paren.split() # members of a constraint list
        constraint_type = members[0]
        if (constraint_type == "constant"):
            node = pydot.Node(members[2], shape="circle", \
                    label=members[1], style="filled", \
                    fillcolor="lightpink")
            G.add_node(node)

# add all non-const constraints to graph
# nonconst contraints are turned into constraint nodes (record type)
# name is compound: sub1-sub2-result
def add_nonconst_nodes(constraints):
    # iter through parenthesized blocks (constraint lists)
    for paren in re.findall("\((.*?)\)", constraints):
        members = paren.split() # members of a constraint list
        # elements: 0=constraint, 1=sub1, 2=sub2, 3=(result of binary constraint)
        constraint_type = members[0]
        # if nonconst, then it's a binary constraint
        if (constraint_type != "constant"):
            sub1 = members[1]
            sub2 = members[2]
            result = members[3]

            if (constraint_type == "adder"):
                # label node as "a1|a2|+|sum"
                constraint_node_label=f"<{sub1}> a1|<op> {constraint_symbols.get(constraint_type)}|<{sub2}> a2| = |<{result}> sum"
            elif (constraint_type == "multiplier"):
                # label node as "m1|m2|*|prod"
                constraint_node_label=f"<{sub1}> m1|<op> {constraint_symbols.get(constraint_type)}|<{sub2}> m2| = |<{result}> prod"
            elif (constraint_type == "powerer"):
                # label node as "base|expt|^|pow"
                constraint_node_label=f"<{sub1}> base|<op> {constraint_symbols.get(constraint_type)}|<{sub2}> expt| = |<{result}> pow"

            # add node to graph regardless of type
            node = pydot.Node(sub1+"_"+sub2+"_"+result, shape="record",\
                    label=constraint_node_label, style="filled", \
                    fillcolor="lightblue")
            G.add_node(node)


# connect each constant node to a constraint
def add_edges(constraints):
    # iter through parenthesized blocks (constraint lists)
    # for each 'current' (i_constraint) node, loop through the rest \
    # of the 'other' (j_constraint) nodes
    paren_blocks = re.findall("\((.*?)\)", constraints)
    for idx,i_constraint in enumerate(paren_blocks):
        # members of constraint list for the 'current' node
        current_list = i_constraint.split()
        # get name for given constraint list aka node
        current_node_name = retrieve_name(current_list)

        # iterate through the rest of the constraint list
        for j_constraint in paren_blocks[idx+1:]:
            other_list = j_constraint.split()
            other_node_name = retrieve_name(other_list)
            connect_nodes(current_node_name, other_node_name, \
                    other_list)

# attempt to connect given nodes
# const constraint: only "iterate" through connector subexp
# binary constraint: iterate through sub1, sub2, result
def connect_nodes(node1_name, node2_name, node2_list):
    if (node2_list[0] == "constant"):
        if node2_name in node1_name:
            G.add_edge(pydot.Edge(src=node1_name+ \
                    ":"+node2_name, dst=node2_name+ \
                        ":"+node2_name))
    else:
        for subexp in node2_list[1:]:
            # build edge if any subexps are found in current node
            if subexp in node1_name:
                G.add_edge(pydot.Edge(src=node1_name+ \
                        ":"+subexp, dst=node2_name+ \
                            ":"+subexp))

# add node for unknown var
def add_ans_node():
    # connect ans to its respective constraint
    G.add_node(pydot.Node("ans", shape="square", style="filled", \
                            fillcolor="palegreen"))
    for node in G.get_nodes():
        node_name = node.get_name()
        if ("ans" in node_name) and (node_name != "ans"):
            # subgraph test
            G.add_edge(pydot.Edge(src=node_name+":ans", dst="ans"))

# retrieve name for node given list of constraint members
def retrieve_name(members):
    # test for const constraint
    if (members[0] == "constant"):
        current_node_name = members[2]
    else:   # constraint node is binary
        current_node_name = members[1]+"_"+members[2]+"_"+members[3]

    return current_node_name

# run program
main()
print(dpath)
