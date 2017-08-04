import xmlrpclib
import argparse
import re

def post_process_table_content(table):
    header_row = re.findall(
        r"(<tr.*?>.*?</tr>)\n*",
        table,
        re.M | re.I | re.S | re.MULTILINE
    )
    if header_row:
        old_header_row = "{0}".format(header_row[0])
        header_row[0] = header_row[0].replace("<td", "<th")
        header_row[0] = header_row[0].replace("</td>", "</th>")
        table = table.replace(old_header_row, header_row[0])
    for link in re.findall(
            r'(https?://[^ ]+[0-9]+)',
            table,
            re.M | re.I | re.S | re.MULTILINE
    ):
        table = table.replace(link, "<a href=\"{0}\">{0}</a>".format(link))
    return table

def pre_extract_tables(content):
    table_map = {}
    counter = 0
    content = re.sub(r'<!-- .*? -->\n*', "", content, re.MULTILINE)
    for table in re.findall(
            r"(<table.*?>.*?</table>)\n*",
            content,
            re.M | re.I | re.S | re.MULTILINE
    ):
        key = "table-2a08be209cd4ee4ce1ff43e08e48158ae12aa92f-{0}".format(counter)
        content = content.replace(table, key)
        counter = counter + 1
        table_map[key] = post_process_table_content(table)
    # print("content: {0}, table_map: {1}".format(content, table_map))
    return content, table_map

def post_insert_tables(content, table_map):
    for key, value in table_map.items():
        content = content.replace(key, value)
    # print("post content: {0}".format(content))
    return content

def main(args):
    client = xmlrpclib.Server(args.confluence_url, verbose = 0)
    auth_token = client.confluence2.login(args.username, args.password)
    page = client.confluence2.getPage(auth_token, args.page_id)
    if args.action == "print-wiki-title":
        print("title: {title}\nurl: {url}".format(
            title=page['title'],
            url=page['url']
        ))
    elif args.action == "print-wiki-content":
        print("{content}".format(
            content=page['content']
        ))
    elif args.action == "store-wiki-content":
        if page['creator'] != args.username:
            print(("The wiki {url} is not created by you\n"
               "To avoid messing up the wiki system\n"
                   "I'd like to not update it. exit!!!").format(
                       url=page['url']
                   ))
        else:
            # print("args.markup is {0}".format(args.markup))
            if args.markup == "true":
                print("Is wiki markup and need to convert to internal storage")
                content, table_map = pre_extract_tables(args.content)
                content = client.confluence2.convertWikiToStorageFormat(
                    auth_token,
                    content
                )
                content = post_insert_tables(content, table_map)
                page['content'] = content
            else:
                print("Is html, no need to convert to internal storage")
                page['content'] = args.content
            print("PAGE content is {0}".format(page['content']))
            result = client.confluence2.storePage(auth_token, page)
            if result:
                print("The content of {0} was updated just now!".format(
                    page['url']
                ))
            else:
                print("Failed to update {0} failed!".format(page['url']))
    else:
        print("error input action")
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
        '-m',
        '--markup',
        type=str,
        help=('markup'),
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

    parser.add_argument(
        '-a',
        '--action',
        type=str,
        help=('action to make'),
        choices=["print-wiki-title", "print-wiki-content", "store-wiki-content"],
        required=True
    )
    args = parser.parse_args()
    main(args)
