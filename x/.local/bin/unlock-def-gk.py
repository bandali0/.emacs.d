#! /usr/bin/env python2
import gnomekeyring as gk
# list_item_ids_sync('login')[0] == 1L
gk.unlock_sync('Default_keyring',gk.item_get_info_sync('login',1L).get_secret())
