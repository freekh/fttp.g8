package models.github

/**
 * Represents a Github Repo(sitory)
 *
 * Read more here: http://developer.github.com/v3/repos/
 *
 * Example of returned JSON from GET /user/watched:
 *
 * {{{[
 * {
 * "url": "https://api.github.com/repos/octocat/Hello-World",
 * "html_url": "https://github.com/octocat/Hello-World",
 * "clone_url": "https://github.com/octocat/Hello-World.git",
 * "git_url": "git://github.com/octocat/Hello-World.git",
 * "ssh_url": "git@github.com:octocat/Hello-World.git",
 * "svn_url": "https://svn.github.com/octocat/Hello-World",
 * "mirror_url": "git://git.example.com/octocat/Hello-World",
 * "id": 1296269,
 * "owner": {
 * "login": "octocat",
 * "id": 1,
 * "avatar_url": "https://github.com/images/error/octocat_happy.gif",
 * "gravatar_id": "somehexcode",
 * "url": "https://api.github.com/users/octocat"
 * },
 * "name": "Hello-World",
 * "description": "This your first repo!",
 * "homepage": "https://github.com",
 * "language": null,
 * "private": false,
 * "fork": false,
 * "forks": 9,
 * "watchers": 80,
 * "size": 108,
 * "master_branch": "master",
 * "open_issues": 0,
 * "false_issues": false,
 * "pushed_at": "2011-01-26T19:06:43Z",
 * "created_at": "2011-01-26T19:01:12Z",
 * "updated_at": "2011-01-26T19:14:43Z"
 * }
 * ]}}}
 *
 */
case class Repo(name: String, description: String, hasIssues: Boolean, openIssues: Long, url: String)
