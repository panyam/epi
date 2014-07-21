
import utils

# EPI 7.1 - Merge sorted lists
def merge_sorted_lists(l1, l2, doubly_linked = False):
    head = tail = None
    while l1 and l2:
        if l1.val < l2.val:
            node = ListNode(l1.val)
            l1 = l1.next
        else:
            node = ListNode(l2.val)
            l2 = l2.next
        if not head:
            head = tail = node
        else:
            tail.next = node
            if doubly_linked: node.prev = tail
            tail = tail.next
    if l1:
        if tail: 
            tail.next = l1
            if doubly_linked: l1.prev = tail
        else:
            head = l1
    else: # if l2:
        if tail: 
            tail.next = l2
            if doubly_linked: l2.prev = tail
        else:
            head = l2
    return head

# EPI 7.6 - Even-Odd Merge
def even_odd_merge(input):
    if not input or not input.next: return input
    heads = [None, None]
    tails = [None, None]
    curr = 0
    while input:
        next = input.next
        input.next = None
        if heads[curr] is None:
            heads[curr] = tails[curr] = input
        else:
            tails[curr].next = input
            tails[curr] = input
        input = next
        curr = (curr + 1) % 2
    if not tails[0]:
        return heads[1]
    tails[0].next = heads[1]
    return heads[0]

head, tail = utils.ll_from_list(xrange(0, 0))
print "Head: " + utils.ll_to_string(head)
print "EvenOddMerge: " + utils.ll_to_string(even_odd_merge(head))
