import xmlrpclib
import argparse


def main(args):
    client = xmlrpclib.Server(args.confluence_url, verbose = 0)
    auth_token = client.confluence2.login(args.username, args.password)
    page = client.confluence2.getPage(auth_token, args.page_id)
    if page['creator'] != args.username:
        print(("The wiki {url} is not created by you\n"
               "To avoid messing up the wiki system\n"
               "I'd like to not update it. exit!!!").format(
                   url=page['url']
               ))
    else:
        page['content'] = args.content
        result = client.confluence2.storePage(auth_token, page)
        if result:
            print("The content of {0} was updated just now!".format(
                page['url']
            ))
        else:
            print("Failed to update {0} failed!".format(page['url']))
    client.confluence2.logout(auth_token)


if __name__ == "__main__" :
    parser = argparse.ArgumentParser()
    parser.add_argument(
        '-u',
        '--username',
        type=str,
        help=('crowd username.'),
        required=True
    )
    parser.add_argument(
        '-p',
        '--password',
        type=str,
        help=('crowd password'),
        required=True
    )
    parser.add_argument(
        '-P',
        '--page_id',
        type=str,
        help=('page id'),
        required=True
    )
    parser.add_argument(
        '-c',
        '--content',
        type=str,
        help=('content to update to wiki page.'),
        required=True
    )
    parser.add_argument(
        '-C',
        '--confluence_url',
        type=str,
        help=('content to update to wiki page.'),
        required=True
    )

    args = parser.parse_args()
    main(args)
