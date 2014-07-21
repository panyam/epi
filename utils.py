
class ListNode(object):
    def __init__(self, val, next = None, prev = None):
        self.val = val
        self.next = next
        self.prev = prev

    def __str__(self):
        return self.__repr__()

    def __repr__(self):
        prev = getattr(self, "prev", None)
        next = getattr(self, "next", None)
        if prev is not None: prev = prev.val
        if next is not None: next = next.val
        return "(%s,%s,%s)" % (prev, self.val, next)

def ll_to_string(ll):
    out = ""
    curr = ll
    while curr:
        if out: out += ", "
        out += str(curr)
        curr = curr.next
    return out

def ll_from_list(values, doubly_linked = False):
    head = tail = None
    for v in values:
        node = ListNode(v, None, None) 
        if not head:
            head = tail = node
        else:
            tail.next = node
            if doubly_linked: node.prev = tail
            tail = node
    return head, tail


