---
title: General Help
description: A page that consists of general troubleshooting and helpful insights.
---

This document serves as a reference for general troubleshooting and helpful insights that do not fall under any particular category of services in this repository. For example, help regarding permissions for the file system would fall into this document.

## Users and Groups

Before addressing matters concerning `uid` (user identification) and `gid` (group identification), we must first understand what they are.

The `UID` is a unique number assigned to each user on the system. Every user account has a corresponding `UID`, which the system uses internally to reference the user. This is how the operating system knows which user is performing an action, such as running a command, accessing a file, or making changes.

Hence, when a user logs in or performs any action, their `UID` is associated with the process they initiate. The `UID` is used to determine the user's permissions and what files or resources they can access. For example, if a user tries to access a file, the system will check the file's permissions and compare them to the UID of the user requesting access to decide whether or not access is granted.

There are special `UIDs` such as `0`, which is reserved for the root user, who is also refered to as the superuser/admin. Other `UIDs` (e.g. 1000, 1001) are assigned to regular users.

To obtain the `UID` run the following,

```shell
    id -u
    # or
    id -u <username>
```

`GID` on the other hand is a unique number assigned to each group on the system. Every group (whether it's a system or user-created group) has its own `GID`, and each user can belong to one or more groups. Groups are used to organize users and set permissions for multiple users at once.

Groups help manage file permissions in a more efficient way. When a file is created, it is assigned a group. The `GID` determines which group has ownership of the file. If a user belongs to a group (determined by their `GID`), they might be able to access certain files or directories that the group has permissions for.

For example, a user might have a primary group (typically with the same name as the user) and additional secondary groups, like a sudo group for administrative tasks.

Similarly to the `UID` the following consists of special `GID`,

- `GID = 0` is typically associated with the root group, which has special permissions for system administration tasks.
- Other `GIDs` are assigned to different groups on the system. For example, sudo might have `GID = 27`, and a user's primary group might have the same `GID` as their `UID`.

To obtain the `GID` run the following,

```shell
  id -g
  # or
  id -g <username>
```


