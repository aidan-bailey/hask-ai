#!/usr/bin/env python3


class FormationTree:
    self.Root = None

    # def insert_node(self, ):


class Node:
    self.Token = None


class OpNode(Node):
    self.children = []

    def __init__(self, operation):
        self.Token = operation


class AtomLeaf(Node):
    def __init__(self, operation):
        self.Token = operation
